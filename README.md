
## Directory Layout

In the main dir, there are a number of subdirs:
 * `bin` - executables (non-test)
 * `cfg` - configuration files
 * `idx` - index files (in subdirs)
 * `log` - LSD log files
 * `script` - Ruby scripts ( `build_all_indices.rb`, `come_down.rb`, `drop_acid.rb` )
 * `src` - lib `.ml[i]` files (and build artefacts)
 * `test` - test `.ml` files (and executables)

## Deployment

#### `cfg/config.yml`

The value of `:idx_root_dir` must be the KAINOA_ROOT/idx.

#### `idx/`

Index files will be created in (subdirs of) this dir.   If you're copying from another machine, you'll need to know where to put them.

#### `script/`

Commands in this dir must be run from KAINOA_ROOT, thus:

    # script/build_all_indices.rb
    # script/come_down.rb
    # script/drop_acid.rb

