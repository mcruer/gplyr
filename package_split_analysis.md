# gplyr Package Splitting Analysis

## Executive Summary

The gplyr package contains **50 exported functions** organized into 8 thematic areas. After thorough analysis, I've identified **5 viable package splits** ranked by priority based on explicit evaluation criteria.

---

## Evaluation Criteria (Weighted)

I'm using the following criteria to evaluate each potential package:

1. **Cohesion (25%)** - Do functions naturally belong together with a single, clear purpose?
2. **Independence (20%)** - Can the package stand alone without heavy internal dependencies on other gplyr functions?
3. **Market Fit (20%)** - Is there a clear audience and use case? Does it solve a real pain point?
4. **Completeness (15%)** - Is it substantial enough to justify a standalone package (not too small)?
5. **Stability (10%)** - Is the functionality stable and unlikely to need breaking changes?
6. **Naming Clarity (5%)** - Can it have an obvious, memorable package name?
7. **CRAN Readiness (5%)** - How close is it to being production-ready?

**Scoring**: Each criterion rated 1-10, weighted by percentage above, final score out of 100.

---

## Option 1: Clipboard Integration Package ⭐ HIGHEST PRIORITY

### Package Name: `clipflow` or `clipr2` or `spreadr`

### Functions (5 total)
- `clip_it()` - Copy dataframe to clipboard
- `paste_it()` - Read dataframe from clipboard
- `clipped_vec()` - Read vector from clipboard
- `clipboard_to_list()` - Convert clipboard to R list format
- `fix_path()` - Convert Windows paths to R format

### Evaluation Scores

| Criterion | Score | Justification |
|-----------|-------|---------------|
| **Cohesion** | 10/10 | Perfect thematic unity - all about clipboard operations |
| **Independence** | 10/10 | Zero dependencies on other gplyr functions; only uses `clipr` package |
| **Market Fit** | 9/10 | Huge audience: anyone moving data between Excel/spreadsheets and R |
| **Completeness** | 7/10 | Small (5 functions) but complete for its purpose |
| **Stability** | 9/10 | Mature functionality, unlikely to need breaking changes |
| **Naming Clarity** | 9/10 | Easy to name: `clipflow`, `spreadr`, `clipr2` all work well |
| **CRAN Readiness** | 9/10 | Already well-documented, minimal dependencies |

**Weighted Score: 9.05/10 (90.5%)**

### Why This Ranks #1

1. **Clear, focused purpose**: Every function serves clipboard integration
2. **No internal dependencies**: Can be extracted with zero refactoring
3. **Massive audience**: Excel → R workflow is extremely common
4. **Unique value**: While `clipr` exists, these are higher-level convenience wrappers
5. **Easy marketing**: "Stop manually saving CSVs - just copy/paste between Excel and R"
6. **Already complete**: Doesn't need additional functions to feel whole

### Dependencies
- External: `clipr`, `readr`, `stringr`, `dplyr`
- Internal: NONE

### Suggested Next Steps
1. Create `clipflow` package
2. Add vignette showing Excel ↔ R workflows
3. Submit to CRAN within 2-4 weeks

---

## Option 2: Pipe Enhancement Package ⭐ SECOND PRIORITY

### Package Name: `pipewhen` or `pipeif` or `condpipe`

### Functions (3 core + 2 potential additions)

**Core Functions:**
- `%pif%` / `pif()` - Conditional pipeline transformation
- `pipe_assign()` - Apply transformation and assign within pipe

**Potential Additions from gplyr:**
- `peek()` / `peek_n()` / `peek_v()` - Non-destructive inspection (these fit the "pipe workflow" theme)

### Evaluation Scores

| Criterion | Score | Justification |
|-----------|-------|---------------|
| **Cohesion** | 9/10 | All enhance pipe workflows, though peek is slightly different purpose |
| **Independence** | 10/10 | No internal gplyr dependencies |
| **Market Fit** | 8/10 | Power users love pipe enhancements; see `magrittr` add-ons popularity |
| **Completeness** | 6/10 | Small (3 functions), but could add peek functions to make it 6 |
| **Stability** | 10/10 | Simple, elegant implementations unlikely to change |
| **Naming Clarity** | 8/10 | `pipewhen` is intuitive; clearly signals purpose |
| **CRAN Readiness** | 8/10 | Well-documented, but would benefit from extensive examples |

**Weighted Score: 8.50/10 (85.0%)**

### Why This Ranks #2

1. **Solves real pain point**: Conditional piping is a common need in tidyverse workflows
2. **Simple, elegant**: Small codebase is easier to maintain
3. **Power user appeal**: Tidyverse enthusiasts will love this
4. **Composability**: Works with any pipe-friendly code
5. **Teaching tool**: Great for demonstrating advanced R programming patterns

### Dependencies
- External: `rlang`, `magrittr`
- Internal: NONE (if peek functions included, adds `tibble`, `dplyr`)

### Suggested Next Steps
1. Create `pipewhen` package with core pipe functions
2. Consider including peek functions for completeness
3. Write comprehensive vignette with complex piping examples
4. Submit to CRAN within 4-6 weeks

---

## Option 3: NA Handling Package ⭐ THIRD PRIORITY

### Package Name: `natools` or `nahandler` or `navault`

### Functions (8 total)

**NA Conversion & Replacement:**
- `na_to_T()` - Convert NA to TRUE
- `na_to_F()` - Convert NA to FALSE
- `replace_with_na()` - Replace pattern matches with NA
- `merge_if_na()` - Replace NA with values from another column

**NA Filtering:**
- `filter_in_na()` - Keep rows with NA in specified columns
- `filter_out_na()` - Remove rows with NA in specified columns

**NA Analysis:**
- `na_review()` - Comprehensive NA diagnostics by column
- `sum_rna()` - Sum with automatic NA removal

### Evaluation Scores

| Criterion | Score | Justification |
|-----------|-------|---------------|
| **Cohesion** | 9/10 | All functions handle NA values; very thematic |
| **Independence** | 7/10 | filter_*_na functions call internal filter_str; needs minor refactoring |
| **Market Fit** | 9/10 | NA handling is a universal R pain point |
| **Completeness** | 8/10 | Good coverage: conversion, filtering, diagnosis |
| **Stability** | 9/10 | NA handling patterns are well-established |
| **Naming Clarity** | 7/10 | `natools` is okay but not exciting |
| **CRAN Readiness** | 7/10 | Would need to extract/refactor filter_str dependency |

**Weighted Score: 8.15/10 (81.5%)**

### Why This Ranks #3

1. **Universal problem**: Every R user deals with NA values
2. **Comprehensive**: Covers detection, conversion, replacement, and diagnosis
3. **Complementary to tidyr**: Adds practical tools beyond `tidyr::drop_na()` and `replace_na()`
4. **Good size**: 8 functions is substantial without being overwhelming
5. **Teaching opportunity**: Can include best practices for NA handling

### Dependencies
- External: `dplyr`, `stringr`, `purrr`, `scales`, `rlang`
- Internal: Needs `filter_str()` extracted or refactored

### Suggested Next Steps
1. Extract `filter_str()` as internal function in new package
2. Create `natools` package
3. Write vignette on NA handling best practices
4. Submit to CRAN within 6-8 weeks

---

## Option 4: Data Inspection Package ⭐ FOURTH PRIORITY

### Package Name: `peek` or `looker` or `inspector`

### Functions (6 total)

**Interactive Inspection:**
- `peek()` - Print transformed object while returning original
- `peek_n()` - Print with row limit
- `peek_v()` - Open in RStudio Viewer
- `pull_cell()` - Extract specific cell value

**Data Profiling:**
- `na_review()` - NA diagnostics (could overlap with natools)
- `count_all()` - Count unique values for all columns

### Evaluation Scores

| Criterion | Score | Justification |
|-----------|-------|---------------|
| **Cohesion** | 8/10 | All about inspecting data, but peek vs. profiling are different modes |
| **Independence** | 10/10 | No internal dependencies |
| **Market Fit** | 7/10 | Useful for interactive analysis, but niche audience |
| **Completeness** | 7/10 | Core is solid but could use more profiling functions |
| **Stability** | 9/10 | Simple implementations |
| **Naming Clarity** | 9/10 | `peek` is perfect - short, memorable, descriptive |
| **CRAN Readiness** | 8/10 | Well-documented, simple code |

**Weighted Score: 8.10/10 (81.0%)**

### Why This Ranks #4

1. **Simple, elegant concept**: "Look at your data without breaking the pipe"
2. **Great name**: `peek` is short and memorable
3. **IDE integration**: RStudio Viewer integration is nice touch
4. **Teaching tool**: Helps users learn pipe debugging
5. **Room to grow**: Could add more profiling/inspection functions

### Dependencies
- External: `dplyr`, `tibble`, `scales` (for na_review)
- Internal: NONE

### Suggested Next Steps
1. Create `peek` package
2. Consider adding more profiling functions (glimpse-like, summary stats, etc.)
3. Write vignette on debugging pipe chains
4. Submit to CRAN within 6-8 weeks

### Note
If you create the `natools` package, move `na_review()` there and keep `peek` focused purely on non-destructive inspection.

---

## Option 5: Data Cleaning Package ⭐ FIFTH PRIORITY

### Package Name: `datacleaner` or `tidyclean` or `sanitizer`

### Functions (10+ total)

**Type Conversion:**
- `to_number()` - Convert columns to numeric
- `to_character()` - Convert columns to character
- `parse_guess_all()` - Reparse all columns using readr

**Date Handling:**
- `fix_dates()` - Standardize various date formats

**NA Handling:**
- `replace_with_na()` - Pattern-based NA replacement
- `merge_if_na()` - Conditional column merging

**Numeric Filtering:**
- `filter_out_numeric()` - Remove rows with numeric values

**String Filtering:**
- `filter_in()` - Keep rows matching string pattern
- `filter_out()` - Remove rows matching string pattern
- `str_filter()` - Filter character vectors

### Evaluation Scores

| Criterion | Score | Justification |
|-----------|-------|---------------|
| **Cohesion** | 6/10 | "Data cleaning" is broad; functions are somewhat disparate |
| **Independence** | 6/10 | Multiple internal dependencies; harder to extract cleanly |
| **Market Fit** | 8/10 | Data cleaning is universal, but less differentiated |
| **Completeness** | 7/10 | Good coverage but overlaps with janitor, tidyr |
| **Stability** | 7/10 | Cleaning logic can be opinionated; may need adjustments |
| **Naming Clarity** | 5/10 | Hard to name without conflicting with `janitor` |
| **CRAN Readiness** | 6/10 | Would need significant refactoring to extract |

**Weighted Score: 6.60/10 (66.0%)**

### Why This Ranks #5

1. **Too broad**: "Data cleaning" encompasses many different operations
2. **Overlap with existing packages**: `janitor`, `tidyr`, and `dplyr` already cover much of this
3. **Internal dependencies**: Functions call each other, making extraction harder
4. **Less differentiation**: Harder to articulate unique value vs. janitor
5. **Better as part of core gplyr**: These functions work well together in a "miscellaneous tools" package

### Dependencies
- External: `dplyr`, `stringr`, `readr`, `janitor`, `lubridate`, `rlang`
- Internal: Multiple (filter_str, quickm, etc.)

### Suggested Next Steps
1. **Don't split this out yet** - keep in core gplyr
2. If you do split it, focus on unique functions not covered by `janitor`
3. Requires most refactoring work

---

## Summary Ranking

| Rank | Package | Score | Priority | Effort | Time to CRAN |
|------|---------|-------|----------|--------|--------------|
| 🥇 #1 | **clipflow** (clipboard tools) | 90.5% | HIGHEST | LOW | 2-4 weeks |
| 🥈 #2 | **pipewhen** (pipe enhancements) | 85.0% | HIGH | LOW | 4-6 weeks |
| 🥉 #3 | **natools** (NA handling) | 81.5% | MEDIUM | MEDIUM | 6-8 weeks |
| #4 | **peek** (data inspection) | 81.0% | MEDIUM | LOW | 6-8 weeks |
| #5 | **datacleaner** | 66.0% | LOW | HIGH | 10-12 weeks |

---

## Recommended Prioritization Strategy

### Phase 1 (Immediate - Next 3 months)
1. **Extract `clipflow`** - Lowest effort, highest impact, clearest value proposition
2. **Extract `pipewhen`** - Small, elegant, appeals to power users

### Phase 2 (Months 3-6)
3. **Extract `natools`** - Requires moderate refactoring but addresses universal pain point
4. **Extract `peek`** - Simple extraction, useful for teaching/debugging

### Phase 3 (Months 6-12)
5. **Consider `datacleaner`** - Only if you can clearly differentiate from `janitor` and justify the effort

### Core gplyr (Keep Together)
After extractions, the remaining functions form a cohesive "advanced dplyr helpers" package:

**Keep in gplyr core (~25-30 functions):**
- Advanced manipulation: `quickm()`, `quicks()`, `mutate_cell()`, `mutate_rowwise()`, `nested_mutate()`
- Joining/merging: `left_join_unique()`, `merge_supersede()`
- Reshaping: `listify_df()`, `df_map()`, `unnest_keep()`
- Categorization: `add_category()`, `add_index()`, `add_temp_column()`
- Utilities: `cloak()`, `uncloak()`, `combine_r_files()`
- String operations that don't fit elsewhere

This creates a **flagship "gplyr" package** with advanced tools, supported by 2-4 smaller, focused companion packages.

---

## Key Success Factors

### What Makes a Good Package Split?

✅ **High cohesion** - Functions clearly belong together
✅ **Low coupling** - Minimal dependencies on other gplyr functions
✅ **Clear audience** - Easy to explain who would use it
✅ **Unique value** - Not redundant with existing CRAN packages
✅ **Good naming** - Package name immediately conveys purpose
✅ **Complete scope** - Feels "whole" not arbitrary

### Red Flags

❌ **"Junk drawer"** - Functions grouped just because they're leftover
❌ **Too small** - <3 functions feels incomplete
❌ **Too coupled** - Requires extracting many internal dependencies
❌ **Unclear purpose** - Hard to write a one-sentence description
❌ **Overlaps existing packages** - Reinventing the wheel

---

## Next Steps

1. **Review this analysis** and confirm the prioritization makes sense for your goals
2. **Start with `clipflow`** - Extract clipboard functions into standalone package
3. **Set up package infrastructure** - Use `usethis::create_package("clipflow")`
4. **Write comprehensive vignettes** - Show real-world Excel ↔ R workflows
5. **Submit to CRAN** - Get first package out the door
6. **Repeat for `pipewhen`** - Build momentum with second extraction
7. **Iterate** - Learn from first two packages before tackling more complex splits

---

## Questions to Consider

1. **Branding**: Do you want all packages under a unified brand (e.g., "gplyverse") or standalone?
2. **Maintenance**: Are you prepared to maintain 4-5 separate packages?
3. **Dependencies**: Should split packages depend on core gplyr, or be fully independent?
4. **Versioning**: How will you handle breaking changes across the ecosystem?
5. **Documentation**: Will you create a pkgdown site for the collection?

---

## Appendix: Functions Not Included in Top 5 Splits

These remain in **core gplyr**:

- `quickm()`, `quicks()` - Multi-column operations
- `mutate_cell()`, `mutate_rowwise()` - Advanced mutation
- `nested_mutate()` - Nested data operations
- `left_join_unique()`, `merge_supersede()` - Advanced joins
- `listify_df()`, `df_map()` - List/dataframe operations
- `unnest_keep()` - List-column handling
- `add_category()`, `add_index()`, `add_temp_column()` - Data organization
- `cloak()`, `uncloak()` - Metadata handling
- `combine_r_files()` - File operations
- `rename_x()` - Column renaming
- `is_in_df()` - Column checking

**Total in core gplyr after extractions: ~25-30 functions**

This creates a sustainable ecosystem:
- **Small, focused packages** for common pain points (clipboard, pipes, NA handling)
- **Core gplyr** as the "power tools" package for advanced users
- **Clear value propositions** for each package
- **Manageable maintenance** burden
