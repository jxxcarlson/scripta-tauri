# The below are instructions for building
# Scripta Desktop for linux and windows, 
# adapted from what I do for MacOS
# you should begin by dowloading the zip
# archive for the source code at 
# https://github.com/jxxcarlson/scripta-tauri/releases

# install rust, MacOS or Linux
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# instructions for Windows installation of Rust
https://forge.rust-lang.org/infra/other-installation-methods.html

# install tauri
cargo install tauri-cli

# build the app
cargo tauri build

# make the zip file for Windows, 
# similarly for Linux
# On MacOS, the is built and placed in
# /Applications -- let's call this
# directory APP, and let OS be 
cp -r APP/scripta-desktop.app .
zip -r scripta-desktop-windows.zip scripta-desktop.app
rm -r scripta-desktop.app
