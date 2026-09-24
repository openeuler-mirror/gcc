# GCC 补丁检查与 CI 接入

这套检查使用 GCC 现有 GNU 风格规则，检查新增、修改的行。
接入后可替代不适合 GCC 的通用格式、命名门禁；构建、回归测试和许可证检查继续保留。

## 检查内容

| Hook | 行为 | 范围 | 默认是否阻断 |
| --- | --- | --- | --- |
| `gcc-patch-gnu-style` | 调用 `contrib/check_GNU_style_lib.py`，输出风格建议 | `gcc/`、`libcpp/`、`include/`、`c++tools/` 下的 C/C++、头文件、`.inc` 补丁，排除测试目录和 `.def` 文件 | 风格问题不阻断；运行异常仍失败 |

GNU 检查保留下划线命名、函数名后的空格和 GNU 的 Tab 使用方式。
其他运行库可能沿用各自上游规范，暂不强制 GNU 风格。
文档、Python、Shell 不套用 C/C++ 风格规则。
`.def` 宏表不参与 pre-commit 的 GNU 风格检查，避免将宏表语法套用普通
C/C++ 的格式规则。
测试及补丁数据可故意包含异常格式，因此 GNU 检查豁免这些路径。
纯删除、二进制和未改变内容的重命名不会触发 GNU 新增行检查。
本 hook 只报告问题，不自动改写文件。
不再添加独立的 `git diff --check` 空白检查。GNU 检查原有的空白规则
仍在上述源码范围内输出建议，与其他 GNU 风格问题一样不阻断提交或 CI。
GNU 检查库通过返回值传递结果（通过为 0，发现问题为 1），自身不退出调用进程。
仓库配置通过 `--advisory` 将 GNU 风格问题作为建议输出，不阻断本地提交或 CI；
`verbose: true` 确保成功状态下仍显示诊断。规则基于文本匹配，可能对合法代码
产生误报，由仓库管理员结合代码语境处理，并在本仓库维护检查项和阻断策略。
只有正常完成检查后的风格问题可以不阻断；引用、依赖、解析或检查程序异常仍失败。
原有 `check_GNU_style.py` 命令行保持严格模式；适配器不传 `--advisory` 时也
返回真实检查状态。后续若要启用 GNU 风格阻断，可由管理员检视后移除配置中的
`--advisory` 参数。

`.pre-commit-config.yaml` 通过 `repo: local` 调用适配脚本，Python 环境由
pre-commit 创建，依赖版本固定在配置中。
适配脚本只检查 pre-commit 传入的当前批次文件，再应用上表中的路径排除规则。
可用 YAML 的 `files`、`exclude` 进一步缩小范围；多批次并行不会重复检查整份补丁。
重命名会同时保留旧、新路径参与比较，避免将未修改的历史行当成新增行。

## 开发者本地使用

在仓库根目录执行，使用 Python 3.10 或更高版本：

```bash
python3 -m venv ../gcc-precommit-venv
. ../gcc-precommit-venv/bin/activate
python -m pip install -r contrib/requirements-pre-commit.txt
pre-commit install --install-hooks

git add <本次修改的文件>
pre-commit run
git commit
```

Windows 可使用同一配置，在 PowerShell 中激活虚拟环境后执行命令。
首次安装会下载 Python 依赖；受网络限制时可为当前命令设置 `PIP_INDEX_URL`。

未提供比较区间时，适配脚本读取暂存区补丁，未暂存修改不会参与检查。
失败时按提示修正新增行，重新暂存再运行即可，无需清理整个文件的历史问题。

检查已提交的改动：

```bash
pre-commit run --hook-stage manual --from-ref <基线提交> --to-ref <待查提交>
```

两个参数都需要提供。区间检查使用提交内容，不使用暂存区和工作区内容。
比较范围与 pre-commit 的文件选择一致，使用 `FROM...TO` 语义：从两者的
共同祖先到待查提交。`--from-ref` 可以传目标分支；目标分支后续独有的改动
不会被反向计入 PR 补丁。若已传入共同祖先作为基线，检查范围不变。
无法找到共同祖先时检查失败；浅克隆请使用下文的 CI 入口补齐历史。
`--all-files` 和 `--files` 不能代替此补丁 hook 所需的比较区间，
它们只限定参与检查的文件，仍检查暂存区或指定区间内的补丁。
传入文件但没有暂存改动时会报错，避免将空补丁误报为通过。
pre-commit 未选中文件时不检查其他文件；直接运行
`python contrib/pre_commit.py gnu-style` 而不传文件时，仍检查完整补丁。
在设置了 `CI` 或 `JENKINS_URL` 的环境中必须提供区间，即使暂存区有改动。
请使用上面的暂存区或区间命令。

## Jenkins / openEuler 门禁接入

仓库内没有当前线上 `check_code` 的服务端配置，添加 YAML 不会自动切换门禁。
CI 管理员需要在现有 GCC 门禁入口中，将调用外部 CodeCheck 的代码检查步骤
改为执行以下命令，并继续沿用现有许可证、双架构构建及状态回写流程：

```bash
# 前置条件：已在 GCC 仓库根目录，HEAD 为本次 PR 的源提交。
# TARGET_BRANCH 由门禁事件传入真实的 PR 目标分支，不写死 master。
# PR_NUMBER 为本次 PR 编号，由门禁事件传入。
python3 -m venv ../gcc-precommit-venv
. ../gcc-precommit-venv/bin/activate
python -m pip install -r contrib/requirements-pre-commit.txt
python contrib/ci_pre_commit.py \
  --target-branch "$TARGET_BRANCH" \
  --repository-url https://gitcode.com/openeuler/gcc.git \
  --pr-ref "refs/merge-requests/$PR_NUMBER/head"
```

CI 脚本会：

1. 固定待查提交 SHA，获取目标分支，计算二者的 merge-base。
2. 如果浅克隆历史不足，同时补取目标分支和指定的 PR 引用，
   每次加深 100 个提交，最多重试三次。
   可用 `--deepen N` 调整；仍无共同基线就报错，不跳过检查。
3. 校验配置，调用 `pre-commit run --from-ref BASE --to-ref HEAD`。
4. 返回检查退出码；依赖或 Git 操作失败也返回非零。

现有 Jenkins 检出流程是 `git init` 后按 URL fetch，通常没有 `origin`。
上面的 `--repository-url` 支持该流程，不要求创建命名 remote。
普通克隆可以省略此参数，默认使用 `origin`；也可用 `--remote` 指定其他
已配置的目标仓库远程名称。目标引用和共同历史已具备时可加 `--no-fetch`。
默认检查 `HEAD`；`--head-ref` 可在本地重放指定 PR 提交。
补取期间若 PR 引用已指向其他提交，脚本报错，需针对新提交重新运行。

框架已通过 `tbranch` 传递目标分支、通过 `common_args.pr_num` 传递 PR 编号；
管理员应将其绑定到上述参数。不要改为仅运行 `pre-commit run --files ...`。
运行 PR 提供的 hook 时使用隔离环境，不向 hook 传递 Jenkins 或 GitCode 凭据。

现有机器人回写记录的名称是 `check_code`。接入时保留这个名称及现有数据结构：
脚本退出码为 0 时映射为成功（现有 `result: 0`），非零映射为失败
（现有 `result: 2`），将输出日志附到详情。不要直接把任意进程退出码当作
机器人枚举值，也不要让代码检查失败阻止许可证和双架构构建继续给出结果。

切换前先在包含本次配置的 PR 分支上运行新入口，确认通过与失败都正确回写；
正式切换时移除旧格式规则的阻断效果，避免新旧检查同时拦截。
按目标分支启用检查，其他维护分支需要先同步配置及脚本；已启用分支缺少
配置或依赖时应报错，不应跳过检查。仅合入本仓配置不会自动修改 Jenkins。
若继续运行 CodeCheck 的其他缺陷检测，应独立配置其有效规则与阻断策略。

## PR #402 的迁移依据

分析对象是提交 `e2110774eb5b302860b1e305b398ffd29efa7157`，基线为
`efae3cfaa6d9201da47bcaf67a6857c142a969fa`。
CodeCheck 使用 `openEuler_cpp_new_2`，9 条告警均为一般级格式或命名问题：

| 原规则 | 告警数 | 迁移处理 |
| --- | --- | --- |
| `G.FMT.02-CPP` 四空格缩进 | 1 | 使用 GCC GNU 规则 |
| `G.FMT.06-CPP` 参数换行对齐 | 2 | 使用 GCC GNU 规则，正确处理 Tab |
| `G.FMT.16-CPP` 函数名后禁止空格 | 2 | 使用 GCC 函数名后保留空格的规则 |
| `G.NAM.03-CPP` 驼峰命名 | 4 | 不要求重命名 GCC 现有下划线标识符 |

报告中的两处参数缩进被按字符数算为 6、5；Tab 展开为 8 列后实际是 41、40，
正好符合对应参数对齐列。这些告警不应通过修改 GCC 风格来消除。
PR 中独立的功能审查意见仍需处理，格式检查通过不代表功能审查完成。

参考：

- [PR #402](https://gitcode.com/openeuler/gcc/pull/402)
- [Jenkins 检查日志](https://ci.openeuler.openatom.cn/job/multiarch/job/openeuler/job/trigger/job/gcc/124/console)
- [CodeCheck 报告](https://www.openlibing.com/apps/entryCheckDashCode/MR_7ba761c37a3c401d8c996b2bc031ea3d/362f43a89f74055e9f41685d535104be?projectId=300024&codeHostingPlatformFlag=gitcode)
- [pre-commit CI 文档](https://pre-commit.com/#usage-in-continuous-integration)

## 适配器回归测试

```bash
python -m pip install -r contrib/requirements-pre-commit.txt unidiff==0.7.5 termcolor==2.4.0
python contrib/test_pre_commit.py -v
```

测试使用临时 Git 仓库，覆盖新增错误拦截、历史问题豁免、Tab 和下划线风格、
暂存区与区间隔离、测试目录、重命名、空格文件名、删除、二进制、无效引用，
分支分叉后的历史行豁免、相同新增错误拦截、无共同祖先失败，
GNU 检查库返回值及原有命令行的标准输出、quickfix 模式，
风格及原有空白建议可见且不阻断、无独立空白门禁及检查异常不被掩盖，
`.def` 宏表的风格豁免、混合补丁中普通源文件的风格检查，
以及浅克隆补齐、无 remote 的 Jenkins 检出、误用文件模式和 CI 失败退出码
传递。不需要构建 GCC。
