# How to Split filtertools into Its Own Repository

The `filtertools/` subdirectory is a complete R package that can be split into its own repository. Here are instructions for doing this while preserving the git history.

## Method 1: Using GitHub (Easiest - Recommended)

This is the simplest approach and works entirely through the GitHub web interface and command line.

### Step 1: Create New Repository on GitHub

1. Go to https://github.com/new
2. Repository name: `filtertools`
3. Description: "Simple and intuitive data filtering tools for R"
4. Choose "Public" (or Private if you prefer)
5. **DO NOT** initialize with README, .gitignore, or license (we already have these)
6. Click "Create repository"

### Step 2: Clone and Copy on Your Local Machine

Open a terminal/command prompt and run:

```bash
# Clone the gplyr repo if you haven't already
git clone https://github.com/mcruer/gplyr.git
cd gplyr

# Switch to the branch with filtertools
git checkout claude/refactor-package-structure-8Jc2P

# Copy the filtertools directory to a new location
cd ..
cp -r gplyr/filtertools filtertools-standalone
cd filtertools-standalone

# Initialize as a new git repo
git init
git add .
git commit -m "Initial commit: filtertools package v0.1.0"

# Connect to the new GitHub repository
git remote add origin https://github.com/mcruer/filtertools.git
git branch -M main
git push -u origin main
```

**Windows PowerShell alternative:**
```powershell
# Clone the gplyr repo if you haven't already
git clone https://github.com/mcruer/gplyr.git
cd gplyr

# Switch to the branch with filtertools
git checkout claude/refactor-package-structure-8Jc2P

# Copy the filtertools directory to a new location
cd ..
Copy-Item -Recurse gplyr\filtertools filtertools-standalone
cd filtertools-standalone

# Initialize as a new git repo
git init
git add .
git commit -m "Initial commit: filtertools package v0.1.0"

# Connect to the new GitHub repository
git remote add origin https://github.com/mcruer/filtertools.git
git branch -M main
git push -u origin main
```

### Step 3: Verify and Clean Up

1. Check that the repository looks good: https://github.com/mcruer/filtertools
2. Test installation: `devtools::install_github("mcruer/filtertools")`
3. If everything works, you can optionally remove the `filtertools/` directory from gplyr

---

## Method 2: Using git filter-repo (Advanced - Preserves Full History)

This method preserves the full git history of files in the filtertools directory. This is more complex but useful if you want to maintain the commit history.

### Prerequisites

Install `git-filter-repo`:
```bash
# On Mac with Homebrew
brew install git-filter-repo

# On Linux
pip3 install git-filter-repo

# On Windows
# Download from https://github.com/newren/git-filter-repo/releases
```

### Step 1: Create a Fresh Clone

```bash
# Make a fresh clone specifically for this operation
git clone https://github.com/mcruer/gplyr.git gplyr-filtertools-split
cd gplyr-filtertools-split

# Switch to the branch with filtertools
git checkout claude/refactor-package-structure-8Jc2P
```

### Step 2: Filter the Repository

```bash
# Extract only the filtertools directory (this rewrites git history)
git filter-repo --subdirectory-filter filtertools/ --force

# At this point, the repository root now contains the filtertools package
# and only commits that touched files in filtertools/ are preserved
```

### Step 3: Create New Repository and Push

```bash
# Create the new repository on GitHub first (see Method 1, Step 1)

# Add the new remote
git remote add origin https://github.com/mcruer/filtertools.git

# Push to the new repository
git branch -M main
git push -u origin main
```

### Step 4: Clean Up

```bash
# Delete the temporary clone
cd ..
rm -rf gplyr-filtertools-split
```

---

## Method 3: Using GitHub's Import Tool (Alternative)

If you prefer not to use the command line:

1. Follow Method 1 to copy the directory and create a new local repo
2. Create the GitHub repository
3. Use GitHub Desktop or your preferred Git GUI to push the local repository to GitHub

---

## After Splitting: Update DESCRIPTION URL

Once the repository is created, the DESCRIPTION file already has the correct URLs:
```
URL: https://github.com/mcruer/filtertools
BugReports: https://github.com/mcruer/filtertools/issues
```

These are already set correctly!

---

## Testing the New Package

After pushing to GitHub, test installation:

```r
# Install from GitHub
devtools::install_github("mcruer/filtertools")

# Load and test
library(filtertools)
library(dplyr)

# Test basic functionality
tibble(x = c("apple", "banana", "cherry")) %>%
  filter_in(x, "app")
```

---

## What's in the Package?

The filtertools package includes:

**7 Functions:**
- `filter_in()` - Keep rows matching string pattern
- `filter_out()` - Remove rows matching string pattern
- `filter_in_na()` - Keep rows with NA in columns
- `filter_out_na()` - Remove rows with NA in columns
- `filter_out_numeric()` - Remove rows with numeric values
- `str_filter()` - Filter character vectors
- `filter_str()` - Internal filtering helper

**Complete Package Structure:**
- ✅ DESCRIPTION with metadata
- ✅ NAMESPACE with exports
- ✅ LICENSE (MIT)
- ✅ README.md with examples
- ✅ Complete documentation (.Rd files)
- ✅ .gitignore and .Rbuildignore

---

## Recommended: Method 1

**I recommend Method 1** because it's:
- Simple and fast (5 minutes)
- Works on any platform (Windows, Mac, Linux)
- No special tools needed
- Clean starting point for the new repo

The full git history of these functions still exists in the gplyr repository, so you're not losing anything important.

---

## Questions?

If you run into any issues, feel free to ask for help!
