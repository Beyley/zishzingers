# zishzingers

A set of tools for working with the fish fingers (FF) bytecode script format used in the mainline LittleBigPlanet franchise.

## Compilation

- Install the latest version of [Zig](https://ziglang.org/) (tested with version `0.12.0-dev.3609+ac21ade66`)
- Run `zig build`, which will place an executable file titled `zishzingers` in `zig-out/bin/`

## Usage

All tools are compiled into the same `zishzingers` executable file, with each tool being available through a different subcommand

### `disasm`
Disassembles script files into a human-readable format

```
# When a script is inside the LBP resource container, just pass the script name
$ zishzingers disasm compressed_script.ff
# When a script is pre-decompressed, you need to specify the compression flags and the asset revision
$ zishzingers disasm -n decompressed_script.ff -c integers -c matrices -c vectors -r 0x272
```

### `compile`
Compiles an A# file into an LBP script file.

```
# TODO: usage instructions
```

Sample scripts are provided in the `samples/` folder.

#### Differences from Aidan's compiler

- s32 literals cannot exceed the size of an s32, in Aidan's compiler, s32 literals may be too big for an s32, as long as they fit in a u32, in this case, it is bitcast back to an s32, this is weird, so im not doing this, use a hex literal.

TODO: move this to the langref once i write it

## Special Thanks

*Massive* shoutouts to [Aidan](https://github.com/ennuo) for the constant help with my reverse engineering efforts, this project would not have been possible if it not for the work they did on [toolkit](https://github.com/ennuo/toolkit) (which I referenced for resource serialization) and craftworld.js (which I referenced for the FF file structure and instruction class types).

Along with shoutouts to [uh wot](https://github.com/uhwot) for early help with the bytecode encoding, and pointing me towards craftworld.js for the file structure.