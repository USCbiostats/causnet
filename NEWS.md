# causnet 0.1.0.9000

## Breaking changes

* `causnet()` and `causnet_stable()` now return every parent in the best
  parent set of each sink. Previously `sink2net()` kept one parent drawn at
  random (`sample.int()`), which made the output depend on the random number
  generator (RNG) state and dropped multi-parent structure. The same data now
  always give the same network with `causnet()`, and with `causnet_stable()`
  when `seed` is fixed. `n_best_parents` keeps its meaning (size of each best
  parent set) and now equals the number of edges each sink contributes.
  Effect on results: for the same best parent sets, the new network contains
  every edge of the old one, plus the other members of multi-parent sets.
  Directed and skeleton recall can therefore only stay the same or increase;
  FDR can move either way. The network does not change when
  `max_parents = 1` or when every best parent set has one member. Earlier
  results also depended on the RNG state, so they could differ between runs.
  On 80 seeded chain datasets (`simdat()`, n = 200, p = 5-10, BIC score,
  `max_parents = 2`), `causnet()` gained 13 edges in 11 datasets and lost
  none; `causnet_stable()` (`tau = 1`, `pi_thr = 0.75`, `B = 50`) was
  unchanged.

* Because `sink2net()` no longer calls `sample.int()`, `causnet()` no longer
  advances the global RNG state at all, and `causnet_stable()` draws fewer
  random numbers than before (its subsampling still uses the RNG). A script
  that calls `set.seed()` once and then alternates data generation with
  `causnet()` / `causnet_stable()` calls gets different random numbers after
  the first call than with earlier versions. Results of such scripts cannot
  be reproduced replicate by replicate from the same seed.

* `causnet()` and `causnet_stable()` now always return a DAG. After a connected
  component was unrolled, `find_best_network()` removed only the rows touching
  the *last, two-node* set of the chain instead of the component's *largest*
  set. The leftover rows were unrolled again as extra, overlapping
  "components", so the output could contain both `a -> b` and `b -> a`
  (2-cycles) and the same edge twice. Each node now belongs to at most one
  component. Genuinely disconnected components are still all returned.
  Effect on results: apart from the random parent draw of the old
  `sink2net()` (see above), the new edge set was a subset of the old one in
  every comparison run so far, so the number of edges and directed recall
  stayed the same or went down. Before, a true adjacency returned in both
  directions always counted as one true positive (plus one false positive),
  whether or not the data supported that orientation. Now only one direction
  is kept. If it is the true one, a false positive disappears. If it is the
  reversed one, a true positive disappears. So directed FDR / SHD can move in
  either direction. In the thesis simulations (p = 20 and 80), the kept
  direction was the reversed one in 113 of 174 such pairs. Directed FDR rose
  in 38 and fell in 24 of the 63 affected cells, and the mean changes were
  small. Skeleton recall stayed the same or fell slightly (it fell in 3 of the
  63 cells); skeleton FDR stayed the same in most cells, fell in 10 and rose
  in 1.

* Taken together, keeping all parents and the DAG fix move the number of
  edges in opposite directions (the DAG fix removes edges, keeping all
  parents adds them), so the net change depends on `max_parents` and on how
  often best parent sets have more than one member. On the 80 seeded chain datasets above,
  `causnet()` went from 636 to 472 directed edges and `causnet_stable()` from
  391 to 345, and no 2-cycles remain. With the `swscore()` change below as
  well, `causnet()` returns 524 edges on these datasets (`causnet_stable()`
  is unchanged).

* A variable can now be the sink of a set of variables when more than
  `max_parents` of its candidate parents are in the set: it then gets the
  best subset of these candidate parents with at most `max_parents` members.
  The internal `swscore()` used to reject such a sink (score `Inf`), whereas
  the published CausNet code uses the best subset; this release follows the
  published code.
  Effect on results: networks can change, mainly when variables have many
  candidate parents (for example with the `alpha` screen of `causnet()`). In
  all comparisons run so far, the total score of the returned network was
  the same or higher, never lower: 1600 small whole-graph runs (p = 4-8),
  216 whole-graph runs (p = 8, 10, 12), 131 phenotype-driven simulation
  settings (p = 10, 20, 50) and the 80 chain datasets above, where
  `causnet()` changed in 41 datasets. The run time can increase: in timing
  runs on phenotype-driven simulation settings with the `alpha` screen
  (p = 30 and 80), by up to 1.9 times (430 s -> 809 s in the slowest
  setting; the largest increase in these runs, not a general bound).

* `score_bge()` now returns the local BGe score
  `score([x][y|x]) - score([x])` (bigger is better), as in the original CausNet
  code. It previously returned the *negated* BGe score of the joint model
  `[x][y|x]`, so the parents' own marginal likelihood stayed in the value and
  the dynamic programme optimised the wrong criterion. It tended to favour the
  largest allowed parent sets and, among sets of equal size, the
  worse-fitting ones: in a check with one true parent and one noise variable,
  it chose both and ranked the noise variable above the true parent. Summed
  over the nodes of a DAG, the new local scores equal
  `bnlearn::score(dag, data, type = "bge")`.
  Any result computed with `score_fun = score_bge` changes. Earlier results
  with `score_bge` came from maximising a different criterion (and, before the
  `sink2net()` fix, keeping one random parent of the chosen set), so they
  should be recomputed rather than read as approximately correct. The size of
  the change was not measured in simulations. The only checks are the
  node-level comparison with bnlearn and the README example, where the new
  code returns the 4 true chain edges.

* `causnet()` and `causnet_stable()` now drop, before the search, every
  variable that has no candidate parents and is not a candidate parent of any
  other variable (see New features). This has the following consequences for
  existing code:
  - A custom `score_fun` is called on the remaining columns only, and the
    column indices it receives refer to those columns. Score functions that
    work with the columns (or column names) of the data they are given, such
    as `score_bic_lm()` and `score_bge()`, are not affected. A score function
    that uses the indices to look something up elsewhere is.
  - With 10 or more variables, dropping variables renumbers the others, and
    the network can then differ from a search on all variables. In checks
    with 216 whole-graph runs (p = 8, 10, 12; alpha rule and stability
    selection) and 131 phenotype-driven simulation settings (p = 10, 20,
    50), 3 runs and 14 settings differed, all only in edge directions, with
    the same total BIC. With at most 9 variables the network is always the
    same.
  - With 10 or more variables, the rows of `network` can also come in a
    different order when the edges are the same, and `n_best_parents` (one
    entry per sink, in the same order) is reordered accordingly. In the 216
    runs above this happened in 18 runs.
  - `causnet` objects created by earlier versions can no longer be passed to
    `return_all_best_networks()` or `return_random_best_networks()`. These
    functions now need the new element `feas` of `mutliple_networks` (see
    Internal), which old objects do not have; for a non-empty network the
    call stops with the error "replacement has 0 rows". Re-run `causnet()`
    or `causnet_stable()` to get a new object.

## Bug fixes

* `causnet()` and `causnet_stable()` no longer fail with
  "subscript out of bounds" (in `find_best_network()`) when the internal sink
  list grows beyond its preallocated `m^4` rows. Clearing the unused tail of the
  list used `seq(n + 1, m^4)`, which leaves stale rows beyond `m^4` and counts
  *downwards* (zeroing valid rows) once `n >= m^4`. Results are unchanged
  whenever the list stays below `m^4` rows. Example: after `set.seed(1234)`,
  `causnet(simdat(n_var = p), alpha = 0.05)` failed for p = 19 and 20 (and
  for no p from 10 to 18); it now completes for both.

* `find_possible_parents_stable()` now always returns a list of length
  `ncol(data)`, with `NULL` for nodes without stable parents (as
  `find_possible_parents()` does). Previously trailing nodes without stable
  parents were deleted from the list, so `causnet_stable()` silently ignored
  them, including as parents of other nodes.

* Descending loops `from:2` in `find_best_network()`, `sink2net()` and
  `best_networks_sink2net()` no longer count upwards when `from` is 1. This did
  not change any result (the loops ended through a `break`).

* Two different sets of variables are no longer merged in the search when
  their sorted indices, written without a separator, give the same string
  (for example `{1, 112}` and `{11, 12}`, both `"1112"`); the internal
  `max_wscore()` kept only the better of the two. The sets are otherwise
  processed in the same order as before, so results change only where such
  sets met, which needs at least 100 variables after the feasSet reduction.

* `find_possible_parents_stable()` and `causnet_stable()` now check that
  `tau` is a single positive finite number (`causnet_stable()` only
  rejected non-numeric and non-positive values).
  `find_possible_parents_stable()` checks `pi_thr` and `tau` before it sets
  `seed`, so an invalid argument no longer changes the RNG state.

* The documentation of `causnet_stable()` now gives the formula for `q` that
  the code uses, `floor(sqrt(tau * (p - 1) * (2 * pi_thr - 1)))`; it showed
  `p` instead of `p - 1`. Documentation only; results are unchanged.

* The documentation of `tau` (`causnet_stable()`,
  `find_possible_parents_stable()`, `README`, `DESCRIPTION`) now says that it
  is a per-node bound on the PFER, and that summed over the screened nodes
  the expected total number of falsely selected candidate parents is at most
  (number of screened nodes) x `tau` (see New features); it said "the
  expected number of falsely selected parents" without saying per node or
  for the whole graph. The help pages and `README` also say that the bound
  holds under the assumptions of Theorem 1 of Meinshausen and Buhlmann
  (2010). In `causnet_stable()` and `find_possible_parents_stable()` it now
  also says that `q` is set to 1 when the formula gives 0 (very small `tau`
  or very few variables), in which case the per-node bound is larger than
  `tau`. Documentation only; results are unchanged.

* The documentation of `causnet()` now lists the three elements of its
  value (`network`, `n_best_parents`, `mutliple_networks`); it said "a list
  with 2 elements". Documentation only.

## New features

* Stability selection for candidate parents: `causnet_stable()` and
  `find_possible_parents_stable()` (new in this release) choose candidate
  parents by stability selection (Meinshausen and Buhlmann, 2010) instead of
  the p-value threshold `alpha` of `causnet()`. `tau` is a per-node bound on
  the per-family error rate (PFER), under the assumptions of Theorem 1 of
  Meinshausen and Buhlmann (2010): the expected number of falsely selected
  candidate parents of each node (each node is screened as its own family of
  p - 1 candidates); summed over the screened nodes, the expected total
  number of falsely selected candidate parents (not edges of the final
  network) is at most (number of screened nodes) x `tau`. `pi_thr` is the
  selection-frequency threshold and `B` the number of subsamples. Both
  functions are exported. (In the fork in which they were first written
  they carried `@export`, but `NAMESPACE` had not been regenerated, so they
  were not exported.)

* Phenotype-driven search: `causnet()` and `causnet_stable()` have two new
  arguments, `outcome` (default `NULL`) and `n_levels` (default 3). With an
  `outcome` (column index or name), candidate-parent links are followed from
  the outcome for at most `n_levels` hops; only the variables reached this way
  keep candidate parents, and only those among the reached variables. This
  includes the variables of the last level, which are not expanded further.
  The network is then learned on that neighbourhood only. With
  `outcome = NULL` the whole graph is searched, as before. For
  `causnet_stable()`, stability selection still runs on all variables; the
  restriction is applied to its result. `n_levels` must be a single whole
  number >= 1; it is checked even when `outcome = NULL`, where it is not
  used.

* feasSet reduction: before the search, `causnet()` and `causnet_stable()`
  drop every variable that has no candidate parents and is not a candidate
  parent of any other variable, as the published CausNet code does. Such a
  variable cannot be part of any edge, so the search finds the same best
  sinks with the same scores, on fewer variables (see Breaking changes: with
  10 or more variables the returned network can differ). This matters for
  speed and memory: the internal sink lists are preallocated with m^4 rows
  for m variables, and the run time of the search grows steeply with m. A
  dataset in which no variable has candidate parents now returns an empty
  network without running the search.

* The phenotype-driven search and the feasSet reduction are ported from the
  simulation and analysis code of the stability-selection thesis that
  proposed `causnet_stable()`. With `outcome` set, `causnet(alpha = 0.01)` and
  `causnet_stable(tau = 1 or 2)` returned exactly the edges of the thesis
  pipeline (with the sink-list and DAG fixes of this release applied to it)
  in all 131 simulation settings compared, before the `swscore()` change:
  `causnet()`, `causnet_stable(tau = 1)` and `causnet_stable(tau = 2)` with
  p = 10 and 20 (20 settings each), and with p = 50 only `causnet_stable()`
  (5 settings with `tau = 1`, 6 with `tau = 2`). `causnet()` was not
  compared with p = 50. The thesis pipeline keeps the earlier `swscore()`
  rule, so with this release 14 of the 131 settings differ from it, all of
  them settings in which that rule rejected sinks; the total BIC is higher
  in 11 of them and the same in 3.

## Deprecated

* `return_all_best_networks()` and `return_random_best_networks()` are
  deprecated. Every call gives a deprecation warning; what they return is
  unchanged. They enumerate the networks that take ONE parent from the
  single best parent set of each sink; these are not genuinely tied best
  networks. Since `causnet()` and `causnet_stable()` now keep every parent of
  that set (see Breaking changes), these networks are no longer alternative
  best networks (example: `causnet()` returns 3 edges on a three-node
  complete DAG, `return_all_best_networks()` two networks of 2 edges each).

## Internal

* New testthat tests for every change above (`test-sink_list_overflow.R`,
  `test-best_network.R`, `test-score_bge.R`, `test-sink2net.R`,
  `test-possible_parents_stable.R`, `test-loop_bounds.R`).
* The demo script `tests/test_causnet_stable.R` (a 50-replicate comparison that
  calls `devtools::load_all(".")`) moved to `dev/test_causnet_stable.R`, which
  is excluded from the build, so that `R CMD check` no longer runs it.
* `find_best_network()` stops with an informative error if the sink list holds
  a row beyond its valid rows (instead of "subscript out of bounds").
* Documentation regenerated with roxygen2 8.0.0.
* `causnet()` and `causnet_stable()` share the search code (new internal
  `causnet_search()`, `causnet_dp()`, `feas_reduce()`, `pheno_expand()`)
  instead of two copies of the pipeline.
* The undocumented element `mutliple_networks` of a `causnet` object now
  describes the reduced problem (node indices 1..m) and has a new element
  `feas` with the column indices of its variables; it is `NULL` when no
  variable has candidate parents. `return_all_best_networks()` and
  `return_random_best_networks()` use `feas`, so they still report column
  indices of `data`; objects created by earlier versions do not work with
  them any more (see Breaking changes).
* New tests `test-feas_reduce.R`, `test-pheno_expand.R`,
  `test-causnet_stable.R`, and a shared `helper-graph.R`.
* New test file `test-return_best_networks.R` (deprecation warning of
  `return_all_best_networks()` / `return_random_best_networks()`, values
  unchanged); the test data `triangle_data()` moved from `test-sink2net.R`
  to `helper-graph.R`. The documentation of the two deprecated functions
  was rewritten (title, description, value, examples wrapped in
  `suppressWarnings()`).
* New test files `test-swscore.R` (the best-subset rule of `swscore()`,
  including a comparison of the total score with a brute-force search over
  complete generational orderings on small datasets, helper in
  `helper-graph.R`) and `test-max_wscore.R` (node-set keys); tests of the
  `tau` check and of the RNG state after an invalid argument in
  `test-possible_parents_stable.R`. The two tests in `test-best_sinks.R`
  have new names and comments that say what they check after the
  `swscore()` change; their expectations are unchanged.
* `DESCRIPTION`: a real `Title` and `Description` replace the template text;
  Tianjiao Cao is added as author; the version is now 0.1.0.9000; the base
  packages `graphics`, `stats` and `utils`, which the code already used,
  are declared in `Imports`.
* `README`: new sections on `causnet_stable()` and on the `outcome` /
  `n_levels` arguments, and a typo fix in the BGe section. `README.md` was
  re-knitted, so its example output now shows the current results (the
  stored output still showed the duplicated second component and the old
  `score_bge()` result).
