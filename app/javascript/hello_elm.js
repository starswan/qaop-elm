// Run this example by adding <%= javascript_pack_tag "hello_elm" %> to the
// head of your layout file, like app/views/layouts/application.html.erb.
// It will render "Hello Elm!" within the page.

import {Elm} from 'Main.elm'
import VirtualAudioContext from './elm-web-audio.js'

document.addEventListener('DOMContentLoaded', () => {
  const target = document.getElementById('applet');
  const load = target.dataset.load;
  const rom = target.dataset.rom;
  const timeInMillis = parseInt(target.dataset.timeinmillis, 10);

  const ctx = new AudioContext();
  const virtualCtx = new VirtualAudioContext(ctx);

  const app = Elm.Main.init({
    node: target,
    flags: {rom: rom, tape: load, timeInMillis: timeInMillis}
  })

  app.ports.toWebAudio.subscribe((nodes) => {
    virtualCtx.update(nodes);
  });

});

// import { Elm } from './Main.elm'
// import VirtualAudioContext from './elm-web-audio.js'
//
// const ctx = new AudioContext()
// const virtualCtx = new VirtualAudioContext(ctx)
//
// const app = Elm.Main.init({
//   node: document.querySelector(...),
//   flags: {
//     // ...
//   }
// })
//
// app.ports.toWebAudio.subscribe((nodes) => {
//   virtualCtx.update(nodes)
// })
