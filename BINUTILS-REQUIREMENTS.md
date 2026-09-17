# binutils 对本仓库新增 AArch64 特性的支持要求

本仓库移植了若干 AArch64 架构特性，编译器侧已完整实现（选项、intrinsic、指令选择），
但**汇编器侧需要相应版本的 binutils 才能把生成的汇编变成目标文件**。本文件记录这些
依赖和实测结果，供后续补齐 binutils 时对照。

不显式打开这些 `-march` 扩展时，编译器行为与本次移植前完全一致，因此这些依赖只影响
主动使用相关扩展的构建。

## 一、依赖清单

| 特性 | GCC 选项 | 汇编器需要认识的 `.arch` 扩展名 | 代表指令 |
| --- | --- | --- | --- |
| FEAT_FPRCVT | `-march=...+fprcvt` | `fprcvt` | `fcvtzs s0, h1`、`scvtf s0, d1` |
| FEAT_FP8 | `-march=...+fp8` | `fp8` | `fcvtn v0.8b, v1.4h, v2.4h`、`fscale` |
| FEAT_FP8FMA | `-march=...+fp8fma` | `fp8fma` | `fmlalb v0.8h, v1.16b, v2.16b` |
| FEAT_FP8DOT2 / DOT4 | `-march=...+fp8dot2` / `+fp8dot4` | `fp8dot2` / `fp8dot4` | `fdot v0.4s, v1.16b, v2.16b` |
| FEAT_F8F32MM / F8F16MM | `-march=...+f8f32mm` / `+f8f16mm` | `f8f32mm` / `f8f16mm` | `fmmla z0.s, z1.b, z2.b` |
| FEAT_SVE_F16F32MM | `-march=...+sve-f16f32mm` | `sve-f16f32mm` | `fmmla z0.s, z1.h, z2.h` |
| FEAT_FAMINMAX | `-march=...+faminmax` | `faminmax` | `famax v0.4s, v1.4s, v2.4s` |
| FEAT_LUT | `-march=...+lut` | `lut` | `luti2 v0.16b, {v1.16b}, v2[0]` |

streaming 变体 `+ssve-fp8fma`、`+ssve-fp8dot2`、`+ssve-fp8dot4` 与对应的非 streaming
扩展同批次进入 binutils，不单列。

## 二、实测结果（2026-09-17）

测试方法见第三节。三个可得的 binutils 版本全部**不认识** fp8 家族与 fprcvt：

| binutils | 来源 | `+fprcvt` | `+fp8` 家族 | `+faminmax` | `+lut` | `+predres2` | `+d128` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 2.40 | Debian 12 | 拒绝 | 拒绝 | 拒绝 | 拒绝 | 拒绝 | 拒绝 |
| 2.41 | **openEuler 24.03-LTS** | 拒绝 | 拒绝 | 拒绝 | 拒绝 | 拒绝 | 拒绝 |
| 2.42 | Ubuntu 24.04 | 拒绝 | 拒绝 | 拒绝 | 拒绝 | 接受 | 接受 |

拒绝形式为 `Error: unknown architectural extension 'xxx'`，随后指令本身也报
`operand mismatch` 或 `unknown mnemonic`。

**最低可用版本未确证**：手上没有 ≥2.43 的环境，无法给出准确的起始版本号。上游
binutils 对 2024 架构扩展的支持集中在 2.43/2.44 两个版本，需要在具备该版本的环境上
用下面的脚本确认后再回填本表。

`+d128` 一行不是本次移植引入的：`TARGET_D128` 与 `__arm_rsr128`/`__arm_wsr128` 在
gcc-14 基线里就有，同样在 openEuler 24.03-LTS 的 2.41 上汇编不过，属于既有缺口。

## 三、验证脚本

在目标环境上运行，输出 `known:` 即为该扩展名可用：

```sh
for e in fprcvt fp8 fp8fma fp8dot2 fp8dot4 f8f32mm f8f16mm sve-f16f32mm \
         faminmax lut predres2 d128; do
  printf '\t.arch armv9-a+%s\n\tnop\n' "$e" > /tmp/e.s
  if as /tmp/e.s -o /tmp/e.o 2>/dev/null; then echo "known:   +$e"
  else echo "UNKNOWN: +$e"; fi
done
```

指令级验证（扩展名可用之后再跑）：

```sh
cat > /tmp/i.s <<'EOF'
	.arch armv9-a+fprcvt+fp8+fp8fma+fp8dot4+faminmax+lut
	fcvtzs	s0, h1
	scvtf	s0, d1
	fcvtn	v0.8b, v1.4h, v2.4h
	fmlalb	v0.8h, v1.16b, v2.16b
	fdot	v0.4s, v1.16b, v2.16b
	famax	v0.4s, v1.4s, v2.4s
EOF
as /tmp/i.s -o /tmp/i.o && echo ASSEMBLER_OK
```

也可以直接用本仓库编出的编译器端到端验证：

```sh
cat > /tmp/t.c <<'EOF'
#include <arm_neon.h>
mfloat8x8_t f (float16x4_t a, float16x4_t b, fpm_t m)
{ return vcvt_mf8_f16_fpm (a, b, m); }
EOF
gcc -O2 -march=armv9-a+fp8 -c /tmp/t.c -o /tmp/t.o && echo END_TO_END_OK
```

## 四、系统寄存器不在此列

`__arm_rsr` / `__arm_wsr` 访问的系统寄存器**没有 binutils 版本要求**，不要照第一节的清单去等。

GCC 从不输出寄存器名，输出的是通用编码形式：

```
__builtin_aarch64_rsr64 ("hfgitr2_el2")  →  mrs x0, s3_4_c3_c1_7
__builtin_aarch64_rsr64 ("vdisr_el3")    →  mrs x0, s3_6_c12_c1_1
```

`s<op0>_<op1>_c<CRn>_c<CRm>_<op2>` 自 Armv8.0 起就是合法语法，任何汇编器都接受。
实测（2026-09-17）：本仓库编译器 `gcc -c` 直接产出 `.o`，同样的编码在 binutils 2.41
上可汇编。

寄存器**名字**只在 GCC 内部用于查 `aarch64-sys-regs.def`，决定
`__arm_rsr("名字")` 在编译期是否被接受，名字不进汇编。因此
`as` 拒绝 `msr sctlr2_el1, x0`（2.41 拒、2.42 接受）这件事，与这条路径无关。

受此影响的特性：FEAT_Debugv8p9、FEAT_SPE_FDS、FEAT_RASv2、FEAT_SCTLR2、
FEAT_SEBEP、FEAT_FGT2、FEAT_STEP2、FEAT_E3DSE —— 它们在 GCC 里只有系统寄存器名，
现在即可使用。FEAT_SPE_EFT 与 FEAT_FPAR 在上游 GCC 中没有任何表示。

## 五、在 binutils 补齐之前

- 编译器侧功能完整：`-march=...+fp8` 等选项可用，intrinsic 可以正常展开，
  `-S` 能看到正确的指令序列，测试用例（全部为 `dg-do compile`）也能通过。
- 但 `-c` / 链接会在汇编阶段失败。因此这些扩展**暂不能出现在发行版默认编译参数里**，
  也不要在 `-mcpu` 的隐含扩展集合中打开。
- 本仓库 `aarch64-cores.def` 中没有任何核声明实现这些特性（最新的 hip12 为 V9_2A），
  所以默认路径不会触发。
