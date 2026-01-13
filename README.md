# GCC for openEuler

GCC for openEuler 编译器是基于开源 GCC 的高性能编译器，GCC for openEuler 与鲲鹏芯片协同，通过编译器技术充分发挥芯片的性能，提升鲲鹏硬件平台上业务的性能体验。除支持开源 GCC 通用功能之外，GCC for openEuler 主要对以下三个方面进行了增强。

- 更丰富的编译算法：提供丰富的优化算法，如内存布局优化、结构体拆分优化、自动矢量化等，大幅提升指令和数据的吞吐量。
- 更灵活的指令流水：适配鲲鹏平台的指令流水优化，发挥鲲鹏架构极致算力。
- 更高效的运算库：深度优化数学库算法，提供丰富的矢量数学函数接口，大幅提升数学计算的效率。

二进制可以从[这里](https://www.hikunpeng.com/developer/devkit/compiler/gcc)下载。

用户指南和新特性，详见[Wiki](https://www.hikunpeng.com/document/detail/zh/kunpengdevps/compiler/ug-hgcc/kunpenghgcc_06_0001.html)。

## 版本节奏

GCC for openEuler 每两年发布一个新版本，选择x.2.y以后的版本。

## 平台支持

GCC for openEuler 当前支持`Linux/AArch64`和`Linux/x86`平台。

## License

GCC for openEuler 使用 GPLv3 协议，请见[License](https://atomgit.com/openeuler/gcc/blob/master/COPYING3)。

## 如何提交贡献

我们非常欢迎开发者提交贡献，如果您发现了一个bug或者有一些想法想要交流，欢迎[发邮件到dev列表](https://www.openeuler.org/zh/community/mailing-list/)或者[提交一个issue](https://atomgit.com/openeuler/gcc/issues)。

## 源码编译指导
### 工具依赖
GCC for openEuler源码构建依赖软件包版本如下：

|  软件包名称 | 版本号 |
| ------------ | ------------ |
| bison          | / |
| chrpath        | / |
| flex           | / |
| gcc            | >= 7 |
| gcc-c++        | >= 7 |
| gdb            | / |
| gettext        | / |
| glibc-headers  | / |
| libtool        | / |
| make           | / |
| python3-sphinx | / |
| sharutils      | / |
| texinfo        | / |
| zlib-devel     | / |
| mpfr-devel     | >= 3.1.0 |
| binutils       | >= 2.31  |
| gmp-devel      | >= 4.1.2 |
| libmpc-devel   | >= 0.8.1 |
| glibc-devel    | >= 2.17  |
### 版本兼容
GCC for openEuler部分特性使用高版本C++标准进行开发，过低版本的gcc可能因为标准兼容性问题无法构建GCC for openEuler。
|  公版GCC版本 | 是否可以构建GCC for openEuler | 不兼容的原因 |
| ------------ | ------------ | ------------ |
|  GCC12.x.x   |    √         |    /         |
|  GCC11.x.x   |    √         |    /         |
|  GCC10.x.x   |    √         |    /         |
|  GCC9.x.x    |    √         |    /         |
|  GCC8.x.x    |    √         |    /         |
|  GCC7.x.x    |    √         |    /         |
|  GCC6.x.x    |    ×         |   GCC for openEuler使用了C++17标准的结构化绑定特性，此特性在GCC 6.x.x中不支持      |
### 构建流程
1. 克隆代码
2. 下载依赖
若环境没有安装依赖，则需要下载相关依赖。
在gcc源码目录下执行脚本下载依赖：
`./contrib/download_prerequisites`
或者在yum源中安装依赖：
`yum install -y libmpc-devel isl-devel flex bison zlib texinfo gmp-devel mpfr-devel make`
3. 新建编译目录，编译产物与源码目录分离
`cd .. && mkdir build && cd build`
4. 执行configure配置
具体配置根据用户需要进行设置，可`gcc -v`参考系统gcc配置。
configure设置为GCC for openEuler的配置路径，prefix设置为安装路径，例如：

    `../configure --prefix={xxx} --mandir=/usr/share/man --infodir=/usr/share/info --enable-shared --enable-threads=posix --enable-checking=release --with-system-zlib --enable-__cxa_atexit --disable-libunwind-exceptions --enable-gnu-unique-object --enable-linker-build-id --with-linker-hash-style=gnu --enable-languages=c,c++,fortran,objc,obj-c++,lto --enable-plugin --enable-initfini-array --disable-libgcj --without-cloog --enable-gnu-indirect-function --build=aarch64-linux-gnu --with-stage1-ldflags=' -Wl,-z,relro,-z,now' --with-boot-ldflags=' -Wl,-z,relro,-z,now' --disable-bootstrap --without-isl --with-multilib-list=lp64 --enable-bolt`

5. 编译与安装
`make -j && make install `
注意安装的路径在configure配置中设置为个人路径，防止覆盖系统GCC路径。

