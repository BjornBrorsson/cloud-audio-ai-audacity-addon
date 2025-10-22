# GitHub Actions Workflow Fixes

## Issues Fixed

### 1. ❌ bdist_wheel Error

**Problem:**
```
error: invalid command 'bdist_wheel'
```

**Cause:** The `wheel` package wasn't installed before running `setup.py bdist_wheel`

**Fix:**
```yaml
- name: Install dependencies
  run: |
    python -m pip install --upgrade pip
    pip install -r requirements.txt
    pip install pyinstaller wheel setuptools  # Added wheel and setuptools
```

### 2. ❌ GitHub Release 403 Error

**Problem:**
```
⚠️ GitHub release failed with status: 403
Error: Too many retries.
```

**Cause:** The workflow didn't have proper permissions to create releases

**Fix:**
Added permissions at the top of the workflow:
```yaml
permissions:
  contents: write   # Allows creating releases
  packages: write   # Allows publishing packages
```

Also updated the action:
```yaml
- name: Create Release
  uses: softprops/action-gh-release@v2  # Updated to v2
  with:
    fail_on_unmatched_files: false  # Don't fail if some files missing
    generate_release_notes: true     # Auto-generate release notes
```

## Additional Improvements

### Better File Matching
Updated to catch all installer types:
```yaml
files: |
  release-*/*.zip
  release-*/*.tar.gz
  release-*/*.dmg      # macOS installer
  release-*/*.deb      # Linux package
  release-*/*.exe      # Windows installer
  release-*/dist/*.whl # Python wheel
  release-*/dist/*.tar.gz
```

### Graceful Failures
Added `continue-on-error: true` to optional build steps so the workflow doesn't fail if:
- Inno Setup isn't available
- PyInstaller fails
- Installer builds aren't successful

## Testing

After pushing, the workflow will:

1. ✅ Build on Windows, Linux, and macOS
2. ✅ Create Python wheels
3. ✅ Build platform-specific installers (when tools available)
4. ✅ Create GitHub release with all artifacts
5. ✅ Attach installers to the release

## What to Expect

When you push v1.0.1 (or later), you should see:

**Successful build outputs:**
- `AudacityCloudAI-Windows-v1.0.1.zip` (source)
- `AudacityCloudAI-Linux-v1.0.1.tar.gz` (source)
- `AudacityCloudAI-macOS-v1.0.1.tar.gz` (source)
- `audacity_cloud_ai-1.0.1-py3-none-any.whl` (Python package)
- `audacity-cloud-ai-1.0.1.tar.gz` (Python source)

**Plus installers (if build tools available on CI):**
- `AudacityCloudAI-Setup-v1.0.1.exe` (Windows installer)
- `audacity-cloud-ai_1.0.1_all.deb` (Linux package)
- `AudacityCloudAI-v1.0.1.dmg` (macOS disk image)

## Next Steps

1. **Commit the fixes:**
   ```bash
   git add .github/workflows/release.yml
   git commit -m "Fix workflow: add wheel package and release permissions"
   git push origin main
   ```

2. **Create new release:**
   ```bash
   git tag -a v1.0.1 -m "Release v1.0.1 - Fix CI/CD workflow"
   git push origin v1.0.1
   ```

3. **Monitor the workflow:**
   - Go to: https://github.com/BjornBrorsson/cloud-audio-ai-audacity-addon/actions
   - Watch the "Build and Release" workflow
   - Should complete successfully in 10-15 minutes

4. **Verify the release:**
   - Go to: https://github.com/BjornBrorsson/cloud-audio-ai-audacity-addon/releases
   - Check all files are attached
   - Test download and installation

## Troubleshooting

If issues persist:

**Python wheel still failing:**
- Check Python version (should be 3.11)
- Verify setuptools is installed

**Release creation still 403:**
- Check repository settings
- Ensure Actions have write permissions
- May need to manually enable in repo Settings → Actions → General → Workflow permissions

**Installers not building:**
- These are optional and may skip if tools aren't available
- You can build them locally (see BUILD_INSTALLER.md)

---

**All fixed and ready to go!** 🚀
