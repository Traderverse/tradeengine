# Pull Request

## Description

<!-- Provide a brief description of the changes in this PR -->

## Related Issue

<!-- Link to the issue this PR addresses -->
Fixes #(issue number)

## Type of Change

<!-- Check all that apply -->

- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring
- [ ] Test improvements

## Which Package(s)?

<!-- Check all affected packages -->

- [ ] tradeengine
- [ ] tradeio
- [ ] tradefeatures
- [ ] tradeviz
- [ ] tradedash
- [ ] trademetrics
- [ ] Documentation only
- [ ] Infrastructure (GitHub Actions, etc.)

## Changes Made

<!-- Describe the changes in detail -->

### Summary
- Change 1
- Change 2
- Change 3

### Technical Details
<!-- Any technical implementation details worth noting -->

## Testing

<!-- Describe the tests you ran to verify your changes -->

### Test Plan
```r
# Example test code
library(testthat)
library(tradeengine)

test_that("new feature works", {
  # Your test
})
```

### Test Results
- [ ] All existing tests pass
- [ ] New tests added
- [ ] Manual testing completed

### Test Coverage
<!-- If applicable, mention test coverage -->
- Current coverage: ___%
- Change in coverage: +/- ___%

## Documentation

<!-- Have you updated the documentation? -->

- [ ] Function documentation (roxygen2)
- [ ] README updated (if needed)
- [ ] Vignette updated/added (if needed)
- [ ] NEWS.md updated
- [ ] Examples added/updated

## Code Quality

<!-- Confirm code quality checks -->

- [ ] Code follows the tidyverse style guide
- [ ] R CMD check passes with 0 errors, 0 warnings
- [ ] Function names are descriptive and consistent
- [ ] Comments are clear and helpful
- [ ] No unnecessary dependencies added

## Breaking Changes

<!-- If this is a breaking change, describe the impact and migration path -->

### Impact
<!-- What will break? -->

### Migration Guide
<!-- How can users update their code? -->

```r
# Old way:
old_function(x)

# New way:
new_function(x)
```

## Screenshots/Output

<!-- If applicable, add screenshots or example output -->

```r
# Example output
```

## Checklist

### Before submitting:
- [ ] I have read the [CONTRIBUTING](../CONTRIBUTING.md) guidelines
- [ ] My code follows the project style guide
- [ ] I have performed a self-review of my code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] Any dependent changes have been merged and published

### For new features:
- [ ] I have added examples
- [ ] I have updated relevant vignettes
- [ ] I have considered backward compatibility
- [ ] I have added appropriate error handling

### For bug fixes:
- [ ] I have added a test that would have caught the bug
- [ ] I have verified the fix with the original issue reporter (if possible)

## Additional Notes

<!-- Any additional information that reviewers should know -->

## Reviewer Guidance

<!-- Help reviewers understand what to focus on -->

Please pay special attention to:
- 
- 

## Post-Merge Tasks

<!-- Tasks to complete after merging, if any -->

- [ ] Update CHANGELOG
- [ ] Announce in Discussions
- [ ] Update documentation site
- [ ] Notify users (if breaking change)

---

**Thank you for contributing to TradingVerse!** 🚀
