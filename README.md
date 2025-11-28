# Blitz

A bytecode compiler and interpreter written in zig.

## Dev

**Build debug**

```sh
make
```

Will generate

<ul>
  <li><code>blitzc-debug</code>: compiler</li>
  <li><code>blitz-debug</code>: virtual machine</li>
  <li><code>bzc-objdump-debug</code>: bytecode disassembler</li>
  <li><code>blitz-diff-debug</code>: bytecode diff utility</li>
</ul>

**Make debug and run tests**

Builds then runs `./test`

```sh
make test
```

**Make prod**

```sh
make prod
```

Will generate

<ul>
  <li><code>blitzc</code>: compiler</li>
  <li><code>blitz</code>: virtual machine</li>
  <li><code>bzc-objdump</code>: bytecode disassembler</li>
  <li><code>blitz-diff</code>: bytecode diff utility</li>
</ul>

**Run all working test files**

Runs from `blitzc-debug` exe in build. Expected to have been built already

```sh
./test
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

By _Jackson Otto_
