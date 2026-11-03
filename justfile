alias rt:=run-tauri
run-tauri:
  cd scripts/GinanTauri && \
  npm run tauri dev

alias i:=install-tauri
install-tauri:
  cd scripts/GinanTauri && \
  npm install

# Build the GinanTauri installers for this machine (output in
# scripts/GinanTauri/src-tauri/target/release/bundle). CI does the same per
# platform in .github/workflows/tauri-build.yml.
alias bt:=build-tauri
build-tauri:
  cd scripts/GinanTauri && \
  npm ci && \
  npm run tauri build

# Cut a release: tags HEAD and pushes the tag, which runs
# .github/workflows/release.yml (draft release -> GinanTauri builds for
# Windows/macOS/Linux attached -> published). Usage: just release v1.2.0
release tag:
  git diff --quiet && git diff --cached --quiet || (echo "Working tree not clean" && exit 1)
  git tag -a {{tag}} -m "Release {{tag}}"
  git push origin {{tag}}
