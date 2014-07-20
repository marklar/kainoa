
# Introduction

Kainoa is a "live" search engine.  With each keystroke, it provides a
(possibly updated) set of results.

It's not unlike Google Suggest, except that Google Suggest suggests
*queries*, while in contrast Kainoa provides actual *results* in real
time.

Because it must provide results with each keystroke, Kainoa is
designed for maximal speed.  In addition, it has some challenging
functional requirements.  Among them:

  * prefix matching of all query terms, and
  * dual ordering of results -- by both:
    + lexical match
    + item popularity

This is a tall order, given the "maximal speed" directive.  So Kainoa
is architected from the ground up to match these specific needs.

## Indexing Constraints

In order to optimize for query speed, we introduce the limitation that
there can be no run-time additions or updates to the index.  If you
wish to index new items or update any existing ones, you must re-index
the entire corpus.

In practice, this limitation is not a problem for our data set, as
additions and updates happen only daily at most.

(Kainoa does, however, support removal of items from the index, since
a simple filtering of results is very efficient.)

By freeing ourselves from the need to update data in our indices, we
enable considerable optimizations.

## Benefits of Mmap-ing: Parallel Processing

This constraint allows us to store not merely all the data, but also
all the data structures that make accessing that data fast, in
memory-mapped files.  From this, we derive some excellent benefits
which would be impossible with dynamic updates to the index.

One such benefit is that any number of different processes, running
entirely in parallel, can use the same set of index files to service
queries, without any need for coordination or any chance for
interference among them.

A second benefit is that the number of such processes is not limited
by memory constraints, as these processes can have very small memory
footprints.  They can home in on the data and access it entirely via
memory-mapped offset files and data files.

## Indexing Expenses

The "no additions or updates" constraint also enables us to benefit
from any additional work we may wish to perform during indexing which
can save us cycles at query time.  So we do.  We perform a lot of data
manipulation during indexing and store the results of same, in order
to optimize query response time.

In particular...

### Fast Term Lookup

First, we store all our indexed terms in sorted order, with a small
hierarchical set of convenient bookmarks to make homing in on the
desired subset very fast.

The sorted storage of the indexed terms gives us excellent spatial
locality of reference when looking for common prefixes, and it makes
the logic for gathering the set of prefix matches extremely efficient.

### Fast Set Operations, No Sorting

Second, we index each result by two different keys, both of which
allow for constant-time (O(1)) lookup:

  * popularity rank, and
  * lexical rank, which more or less means "the shorter, the better"

Then, crucially, for each indexed term, we compute and store two
separate sets of pre-sorted result keys -- one for each type of rank.
That is, for each indexed term, we store:

  * all matching results, sorted by their popularity rank, and
  * all matching results, this time sorted by their lexical rank

Presorting these keys (ranks) is the fundamental source of
optimization of the system.  It allows us two extremely valuable
short-cuts:

  * We can perform set intersection (or union) on *ordered* sets, an
    operation which is (better than) linear in the number of results.

  * We need never perform any sorting at all at query time.  That's
    good, because sorting is generally O(n log n).


## Code Design

Kainoa does not perform indexing.  Kainoa is only the *search* code,
which uses the indices.

To understand how Kainoa works depends on understanding both:

  * the memory-mapped data structures it uses, as well as
  * the strategies it employs to gather prefix matches and perform set
    operations on result IDs

In Types.hs, you'll find the definitions of (nearly) the data types
Kainoa uses.  They're are really two principle types:

  * Domain, a data structure which (via the many data types of which
    it's composed) provides access to the mmap-ed data, and

  * Query, which represents a single incoming query, and which drives
    the set operations to produce a ResultSet.


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

Index files will be created in (subdirs of) this dir.  If you're
copying from another machine, you'll need to know where to put them.

#### `script/`

Commands in this dir must be run from KAINOA_ROOT, thus:

    # script/build_all_indices.rb
    # script/come_down.rb
    # script/drop_acid.rb

