fn main() {
    remove_stale_resources_file();
    tauri_build::build()
}

/// The bundle resource maps to `resources/default_config.yaml`, which needs
/// `target/<profile>/resources` to be a directory. Earlier versions of this
/// crate mapped it to `"resources/"`, which made tauri-build copy
/// default_config.yaml to a *file* at that path, so a target dir built by one
/// of them fails with "File exists (os error 17)" (183 on Windows). Delete the
/// stale file so an existing checkout builds without a `cargo clean`.
fn remove_stale_resources_file() {
    // OUT_DIR is target/<profile>/build/<pkg>-<hash>/out, the same anchor
    // tauri-build uses to find where it copies resources.
    let Some(out_dir) = std::env::var_os("OUT_DIR") else {
        return;
    };
    let Some(profile_dir) = std::path::Path::new(&out_dir).ancestors().nth(3) else {
        return;
    };
    let stale = profile_dir.join("resources");
    if stale.is_file() {
        let _ = std::fs::remove_file(&stale);
    }
}
