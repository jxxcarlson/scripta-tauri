
## Commands

cargo tauri dev
npx elm-watch hot
npx serve public

## Links

https://velociraptor.run
https://dev.to/selbekk/setting-up-a-development-environment-in-elm-4gpf
https://github.com/obsidiansystems/obelisk#who-should-consider-using-it
https://github.com/fjvallarino/monomer/issues/204
https://dmitryrogozhny.com/blog/using-elm-with-typescript-together
https://kickstartcoding.com/learning-center/articles/building-debug-trainer-deskop-app-tauri/
https://itnext.io/from-javascript-to-typescript-to-elm-5c36fca70b4a
https://elm-ts-interop.com/setup
https://www.bekk.christmas/post/2020/11/types-without-borders-isn't-enough
https://socket.dev/npm/package/elm-ts-interop
https://elm-ts-interop.com/setup/
https://github.com/halfzebra/create-elm-app
https://www.humio.com/blog/why-we-chose-elm-for-humio-s-web-ui/
https://functional.works-hub.com/learn/getting-started-with-elm-4c6bc
https://francium.cc/blog/a-few-thoughts-about-elm/
https://wiki.nikiv.dev/programming-languages/elm/


## TAURI

http://mbpfefferle.com/2016/10/14/elm-interop (***)
https://github.com/kickstartcoding/debug_trainer_app/blob/main/js/app.ts (***)
import { readTextFile, writeFile } from '@tauri-apps/api/fs'

Lars Fabian:
import is actually a javascript (ecmascript 6) thing so it will work in js too, but realistically only with a frontend bundler like vite, webpack (idk if elm needs one too).
If you don't have a bundler you'd need to use the global tauri api like window.__TAURI__.fs.readTextFile()

## PORTS

https://lengrand.fr/a-short-introduction-to-ports-and-flags-in-elm/
https://thoughtbot.com/blog/bridging-elm-and-javascript-with-ports

## JS Stuff

https://javascript.info/promise-basics

## TO DO

exposing fs to the front end is ultimately less secure, so if you can help it, i would recommend making an invoke call to the rust backend to check for those files and do that work in Tauri, then just return a boolean to the frontend. Otherwise, you could end up with security issues if someone were to inject some frontend js. I'm not sure exactly what frontend you're using but say you're using es6. it would look like this:

```
frontend.js
import { invoke } from "@tauri-apps/api/tauri"

invoke("check_file", "file_a.txt")
  .then(exists => console.log("file exists: ", exists))
  .catch(err => console.error("unable to check for file: ", err)) 
```

```
main.rs
use std::path::Path;

#[tauri::command]
fn check_file(file_name: String) -> bool {
    Path::new(file_name).exists()
}

tauri::Builder::default()
    // This is where you pass in your commands
    .invoke_handler(tauri::generate_handler![check_file])
    .run(tauri::generate_context!())
    .expect("failed to run app");
```

## Comments on Medium

Looks good so far!

Only 3 small things

- A really common issue people have is that they are missing system dependencies (like xcode-select --install on macos etc)

- "Install the Tauri toolchain: cargo install tauri-app then cargo install tauri-cli." -> The first command is not a valid command, because tauri-app doesn't exist, it's cargo install create-tauri-app which is our bootstrapper (like create-react-app etc, basically a fancy tauri init including frontends), and therefore not necessary for your setup

- cargo tauri dev -- --debug is this --debug for elm? because tauri itself is always in debug mode in tauri dev
Oh and for the Acknowledgements: my name is Fabian-Lars (that's my irl first name btw) - buuut while appreciated, it's really not necessary :)
Of course i can only talk about the Tauri stuff, never worked with elm ✌️