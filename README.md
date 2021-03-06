# Overview

This plugin uses `objdump` to extract symbols from a binary. It does
so by parsing the `objdump` output. It makes several assumptions:

1) `objdump` does not output raw instructions. This is currently
fulfilled by defaulting to the `--no-show-raw-inns` option when
running objdump.

2) Lines that are left flush as "<number>: <name>" signify the start
of a <name> at address <number>.

3) Lines of the form "\s\s<number>: <insn>" --- that is, start with
two spaces --- specify that an instruction <insn> exists at address
<number>

4) `objdump` is located at `/usr/bin/objdump`.  You can change this
with the `--objdump-path=<path>` option.

*NOTE:* We do not extract anything other than symbol names.  In
particular, we do not extract instructions using objdump at this
time.

# Example runs:

To use `objdump`` to extract and view address ranges for symbol names:
```sh
$ bap --symbolizer=objdump --dump-symbols <executable>
```

To use the internal extractor and *not* use `objdump`:
```sh
$ bap --symbolizer=internal --dump-symbols
```

To see help, including other options:
```sh
$ bap --objdump-help
```

# Compilation and Installation
Just run `make`. Note that `make` will also install the plugin

# Debugging
The first thing you should try is invalidating your BAP cache:
```sh
$ bap --cache-clean
```

# Who to blame?
David Brumley <dbrumley@cmu.edu>
