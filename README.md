# Blitz

A bytecode compiler and interpreter written in zig.

## Dev

**Build debug**

```sh
zig build
```

Will generate

<ul>
  <li><code>blitzc-debug</code>: compiler</li>
  <li><code>blitz-debug</code>: virtual machine</li>
  <li><code>bzc-objdump-debug</code>: bytecode disassembler</li>
  <li><code>blitz-diff-debug</code>: bytecode diff utility</li>
</ul>

**Make prod**

```sh
zig build -Dprod
```

Will generate

<ul>
  <li><code>blitzc</code>: compiler</li>
  <li><code>blitz</code>: virtual machine</li>
  <li><code>bzc-objdump</code>: bytecode disassembler</li>
  <li><code>blitz-diff</code>: bytecode diff utility</li>
</ul>

**Run all test files**

Runs from `blitzc-debug` exe. Expected to have been built already

```sh
./test
```

**Compile debug and run all test files**

Compiles with `zig build` in debug mode. Runs from `blitzc-debug` exe.

```sh
./test -c
```

**Run specific test file**

Runs from `zig run ...`

```sh
./test [file name]
```

**Run a file that matches a pattern**

Use the -p flag, runs from `zig run ...`

```sh
./test -p structs
```

If you clone to experiment, it might be helpful to source the env.sh file. It adds the bin directory to the path so you have easier access to the executables.

You must also set the environment variable `BLITZ_DIR` to the root directory of the blitz source

Example:

```sh
BLITZ_DIR = "/home/name/code/blitz"
```

By _Jackson Otto_
