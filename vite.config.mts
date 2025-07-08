import { defineConfig } from 'vite'
import RubyPlugin from 'vite-plugin-ruby'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  plugins: [
    RubyPlugin(),
    elmPlugin(),
  ],
})


//
// const config = {
//   build: {
//     assetsInlineLimit: 24576
//   },
//   plugins: [elmPlugin()]
// }
//
// export default config