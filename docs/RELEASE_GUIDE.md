# Release Guide

## Creating a New Release

### Automatic Release (Recommended)

1. **Tag a new version:**
   ```bash
   git tag -a v1.0.0 -m "Release v1.0.0"
   git push origin v1.0.0
   ```

2. **GitHub Actions will automatically:**
   - Run all tests
   - Build packages for Windows, Linux, and macOS
   - Create a GitHub Release with release notes
   - Upload distribution files
   - (Optional) Publish to PyPI if configured

### Manual Release

1. **Update version in `src/version.py`:**
   ```python
   __version__ = "1.0.1"
   ```

2. **Update CHANGELOG.md:**
   Add new version section with changes

3. **Commit changes:**
   ```bash
   git add src/version.py CHANGELOG.md
   git commit -m "Bump version to 1.0.1"
   git push
   ```

4. **Create and push tag:**
   ```bash
   git tag -a v1.0.1 -m "Release v1.0.1"
   git push origin v1.0.1
   ```

5. **Trigger release workflow:**
   - Go to Actions tab on GitHub
   - Select "Build and Release" workflow
   - Click "Run workflow"
   - Enter version number

## Release Checklist

Before creating a release:

- [ ] All tests passing
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Version number bumped in `src/version.py`
- [ ] No uncommitted changes
- [ ] Feature branch merged to main

## Version Numbering

We follow [Semantic Versioning](https://semver.org/):

- **MAJOR** version (1.x.x): Incompatible API changes
- **MINOR** version (x.1.x): New features, backwards compatible
- **PATCH** version (x.x.1): Bug fixes, backwards compatible

Examples:
- `v1.0.0` - Initial release
- `v1.0.1` - Bug fix
- `v1.1.0` - New feature added
- `v2.0.0` - Breaking changes

## Distribution Files

Each release includes:

### Windows
- `AudacityCloudAI-Windows-vX.X.X.zip`
  - Complete source code
  - Installation scripts
  - Documentation

### Linux
- `AudacityCloudAI-Linux-vX.X.X.tar.gz`
  - Complete source code
  - Installation scripts
  - Documentation

### macOS
- `AudacityCloudAI-macOS-vX.X.X.tar.gz`
  - Complete source code
  - Installation scripts
  - Documentation

### Python Package (PyPI)
- `audacity-cloud-ai-X.X.X.tar.gz` (source)
- `audacity_cloud_ai-X.X.X-py3-none-any.whl` (wheel)

## Post-Release Steps

After creating a release:

1. **Verify release on GitHub:**
   - Check release notes are correct
   - Test download links
   - Verify all artifacts are present

2. **Update documentation:**
   - Update installation instructions if needed
   - Update version references

3. **Announce release:**
   - GitHub Discussions
   - Audacity forums
   - Social media
   - Reddit r/audacity

4. **Monitor for issues:**
   - Check GitHub Issues
   - Respond to user feedback

## PyPI Publishing (Optional)

To enable automatic PyPI publishing:

1. **Create PyPI account:**
   - Go to https://pypi.org/
   - Register an account
   - Verify email

2. **Create API token:**
   - Go to Account Settings → API tokens
   - Create a new token with scope: "Entire account"
   - Copy the token (starts with `pyp-`)

3. **Add token to GitHub Secrets:**
   - Go to repository Settings → Secrets and variables → Actions
   - Click "New repository secret"
   - Name: `PYPI_API_TOKEN`
   - Value: Your PyPI token
   - Click "Add secret"

4. **Next release will automatically publish to PyPI!**

Users can then install via:
```bash
pip install audacity-cloud-ai
```

## Hotfix Release

For urgent bug fixes:

1. **Create hotfix branch:**
   ```bash
   git checkout -b hotfix/1.0.1 main
   ```

2. **Make fixes and test**

3. **Update version and changelog**

4. **Merge to main:**
   ```bash
   git checkout main
   git merge hotfix/1.0.1
   ```

5. **Tag and push:**
   ```bash
   git tag -a v1.0.1 -m "Hotfix: Critical bug fix"
   git push origin main v1.0.1
   ```

## Rollback a Release

If a release has critical issues:

1. **Delete the tag:**
   ```bash
   git tag -d v1.0.1
   git push origin :refs/tags/v1.0.1
   ```

2. **Delete the GitHub release:**
   - Go to Releases
   - Click on the release
   - Click "Delete this release"

3. **Fix the issues**

4. **Create a new release with bumped patch version**

## Testing a Release Locally

Before pushing a tag:

1. **Build distribution:**
   ```bash
   python setup.py sdist bdist_wheel
   ```

2. **Test installation:**
   ```bash
   pip install dist/audacity_cloud_ai-1.0.0-py3-none-any.whl
   ```

3. **Test CLI:**
   ```bash
   audacity-cloudai --help
   ```

4. **Test GUI:**
   ```bash
   audacity-cloudai-gui
   ```

5. **If all works, proceed with release!**

---

**Happy releasing!** 🚀
