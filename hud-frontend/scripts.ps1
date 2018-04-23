function global:build() {
    Remove-Item build\* -Recurse -Force
    robocopy static build /s /e
    pulp browserify --optimise --to ./build/main.js
}
