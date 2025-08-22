# 🔧 GitHub Actions Workflow Fix Guide

## Quick Fix Summary

Your GitHub Actions workflows are likely failing due to common configuration issues. Here's how to fix them:

### 1. **Immediate Actions** ⚡

1. **Create the main workflow file** at `.github/workflows/main.yml` (use the "Fixed GitHub Actions Main Workflow" artifact above)
2. **Create the minimal test workflow** at `.github/workflows/minimal-test.yml` (use the "Minimal Test Workflow" artifact above)
3. **Run the minimal test first** to verify basic functionality

### 2. **Common Failure Causes & Solutions** 🎯

| Issue | Symptoms | Fix |
|-------|----------|-----|
| **Missing Dockerfile** | Docker build fails | Create `docker/Dockerfile` (provided in artifacts) |
| **Invalid YAML syntax** | Workflow won't start | Use YAML validator, check indentation |
| **Missing permissions** | API calls fail | Add proper `permissions:` blocks |
| **Timeout issues** | Jobs hang | Add `timeout-minutes:` to all jobs |
| **Missing dependencies** | Build steps fail | Add proper setup actions (`setup-dotnet`, etc.) |
| **Wrong file paths** | Files not found | Use relative paths, check case sensitivity |

### 3. **Step-by-Step Fix Process** 📋

#### Step 1: Replace Your Main Workflow
```bash
# Create the workflows directory if it doesn't exist
mkdir -p .github/workflows

# Copy the main workflow (from the artifact above)
# Save as .github/workflows/main.yml
```

#### Step 2: Add the Minimal Test
```bash
# Copy the minimal workflow (from the artifact above)  
# Save as .github/workflows/minimal-test.yml
```

#### Step 3: Add Configuration Files
```bash
# Create .yamllint.yml (from the config artifact above)
# Create .github/dependabot.yml (from the config artifact above)
# Create .github/labeler.yml (from the config artifact above)
```

#### Step 4: Test and Verify
1. Commit and push the changes
2. Go to **Actions** tab in your repository
3. Run the "Minimal Test (Always Passes)" workflow first
4. If it passes, run the main workflow
5. Check the troubleshooting workflow if issues persist

### 4. **File Structure You Should Have** 📁

```
.github/
├── workflows/
│   ├── main.yml                 # Main CI/CD pipeline
│   ├── minimal-test.yml         # Always-passing test
│   ├── docker.yml               # Docker build workflow
│   ├── lint.yml                 # Code quality checks
│   └── troubleshoot.yml         # Diagnostic workflow
├── dependabot.yml               # Dependency updates
├── labeler.yml                  # PR auto-labeling
└── mlc_config.json             # Link checking config
.yamllint.yml                   # YAML linting rules
docker/
└── Dockerfile                  # Docker build instructions
```

### 5. **Debugging Workflow Failures** 🔍

#### Check These First:
1. **YAML Syntax**: Use [YAML Validator](https://yamllint.readthedocs.io/en/stable/)
2. **Indentation**: Ensure consistent 2-space indentation
3. **Required Fields**: Every workflow needs `name`, `on`, and `jobs`
4. **Permissions**: Add appropriate `permissions:` blocks

#### Common YAML Errors:
```yaml
# ❌ Wrong - inconsistent indentation
on:
  push:
    branches: [ main ]
   pull_request:  # Wrong indentation

# ✅ Correct - consistent indentation  
on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
```

#### Test Your Workflow Syntax:
```bash
# Install yamllint to check syntax
pip install yamllint

# Check your workflow files
yamllint .github/workflows/
```

### 6. **Repository Settings to Check** ⚙️

1. **Actions Permissions**:
   - Go to Settings > Actions > General
   - Ensure "Allow all actions and reusable workflows" is selected

2. **Workflow Permissions**:
   - Set "Workflow permissions" to "Read and write permissions"
   - Check "Allow GitHub Actions to create and approve pull requests"

3. **Secrets Configuration**:
   - Go to Settings > Secrets and variables > Actions
   - Add any required secrets (DOCKER_USERNAME, etc.)

### 7. **Monitoring and Maintenance** 📊

#### Regular Tasks:
- **Weekly**: Check workflow runs for failures
- **Monthly**: Update workflow dependencies
- **Quarterly**: Review and optimize workflow performance

#### Key Metrics to Monitor:
- Success rate (aim for >95%)
- Average run time
- Resource usage
- Dependency updates needed

### 8. **Advanced Troubleshooting** 🔬

If workflows still fail after basic fixes:

1. **Run the troubleshooting workflow** (provided in artifacts)
2. **Check runner logs** for specific error messages  
3. **Verify network connectivity** from GitHub runners
4. **Test locally** with act or similar tools
5. **Contact GitHub Support** if platform issues are suspected

#### Enable Debug Logging:
Add these secrets to get more detailed logs:
- `ACTIONS_STEP_DEBUG`: `true`
- `ACTIONS_RUNNER_DEBUG`: `true`

### 9. **Best Practices Going Forward** ✨

1. **Always use timeouts** (`timeout-minutes: 15`)
2. **Handle failures gracefully** (`continue-on-error: true` where appropriate)
3. **Use caching** to speed up workflows
4. **Keep workflows simple** - complex workflows are harder to debug
5. **Regular maintenance** - update dependencies and actions versions

### 10. **Quick Validation Checklist** ✅

Before committing workflow changes:

- [ ] YAML syntax is valid
- [ ] All required files exist
- [ ] Permissions are properly set
- [ ] Timeouts are configured
- [ ] Error handling is in place
- [ ] Dependencies are up to date
- [ ] Secrets are configured (if needed)
- [ ] File paths are correct

---

## 🚀 Ready to Go!

After implementing these fixes:

1. **Start with the minimal test workflow** to verify basics
2. **Gradually enable more complex workflows** as you confirm they work
3. **Monitor the Actions tab** for ongoing health
4. **Use the troubleshooting workflow** whenever issues arise

Your ALN Programming Language repository should now have robust, reliable GitHub Actions workflows! 🎉

---

### Need More Help?

- Check the individual workflow logs for specific errors
- Use the troubleshooting workflow to diagnose issues
- Review GitHub Actions documentation for advanced features
- Consider opening an issue if problems persist
