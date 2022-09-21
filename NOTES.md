
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
https://thoughtbot.com/blog/bridging-elm-and-javascript-with-ports
https://socket.dev/npm/package/elm-ts-interop
https://elm-ts-interop.com/setup/
https://github.com/halfzebra/create-elm-app
https://www.humio.com/blog/why-we-chose-elm-for-humio-s-web-ui/
https://functional.works-hub.com/learn/getting-started-with-elm-4c6bc
https://francium.cc/blog/a-few-thoughts-about-elm/
https://wiki.nikiv.dev/programming-languages/elm/


http://mbpfefferle.com/2016/10/14/elm-interop (***)
https://github.com/kickstartcoding/debug_trainer_app/blob/main/js/app.ts (***)
import { readTextFile, writeFile } from '@tauri-apps/api/fs'
Lars Fabian:
import is actually a javascript (ecmascript 6) thing so it will work in js too, but realistically only with a frontend bundler like vite, webpack (idk if elm needs one too).
If you don't have a bundler you'd need to use the global tauri api like window.__TAURI__.fs.readTextFile()
