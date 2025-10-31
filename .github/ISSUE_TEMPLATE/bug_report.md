---
name: Bug report
about: Create a report to help us improve
title: '[BUG] '
labels: bug
assignees: ''

---

## Which Package?

<!-- Check all that apply -->

- [ ] tradeengine
- [ ] tradeio
- [ ] tradefeatures
- [ ] tradeviz
- [ ] tradedash
- [ ] trademetrics
- [ ] Other (please specify)

## Describe the Bug

<!-- A clear and concise description of what the bug is -->

## To Reproduce

Steps to reproduce the behavior:

```r
# Your code here
library(tradeengine)

# Example:
data <- generate_synthetic_data(100)
# ... rest of your code
```

**Expected behavior:**
<!-- What did you expect to happen? -->

**Actual behavior:**
<!-- What actually happened? -->

## Error Message

```
Paste the complete error message here
```

## System Information

```r
# Please run this and paste the output:
sessionInfo()
```

**Package versions:**
```r
# Run this:
packageVersion("tradeengine")
packageVersion("tradeio")
# ... etc
```

## Additional Context

<!-- Add any other context about the problem here -->

- Screenshots (if applicable)
- Data characteristics (size, time period, symbols)
- Any workarounds you've tried

## Reproducible Example

<!-- 
If possible, provide a minimal reproducible example using built-in data.
This helps us fix the bug faster!
-->

```r
library(tradeengine)

# Minimal example that reproduces the bug
# Use generate_synthetic_data() or built-in datasets if possible
```

## Checklist

- [ ] I have searched existing issues to make sure this isn't a duplicate
- [ ] I have included a reproducible example
- [ ] I have included my session info
- [ ] I have checked that my packages are up to date
