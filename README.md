# GCC for openEuler

## GCC for openEuler 10.3.1

GCC for openEuler 编译器是基于开源 GCC 的高性能编译器，GCC for openEuler 与鲲鹏芯片协同，通过编译器技术充分发挥芯片的性能，提升鲲鹏硬件平台上业务的性能体验。除支持开源 GCC 通用功能之外，GCC for openEuler 主要对以下三个方面进行了增强。

- 更丰富的编译算法：提供丰富的优化算法，如内存布局优化、结构体拆分优化、自动矢量化等，大幅提升指令和数据的吞吐量。
- 更灵活的指令流水：适配鲲鹏平台的指令流水优化，发挥鲲鹏架构极致算力。
- 更高效的运算库：深度优化数学库算法，提供丰富的矢量数学函数接口，大幅提升数学计算的效率。

二进制可以从[这里](https://www.hikunpeng.com/developer/devkit/compiler/gcc)下载。

用户指南和新特性，详见[Wiki](https://support.huaweicloud.com/ug-hgcc-kunpengdevps/kunpenghgcc_06_0001.html)。

## 版本节奏

GCC for openEuler 每两年发布一个新版本，选择x.2.y以后的版本。

## 平台支持

GCC for openEuler 当前支持`Linux/AArch64`和`Linux/x86`平台。

## License

GCC for openEuler 使用 GPLv3 协议，请见[License](https://gitee.com/openeuler/gcc/blob/master/COPYING3)。

## 如何提交贡献

我们非常欢迎开发者提交贡献，如果您发现了一个bug或者有一些想法想要交流，欢迎[发邮件到dev列表](https://www.openeuler.org/zh/community/mailing-list/)或者[提交一个issue](https://gitee.com/openeuler/gcc/issues)。