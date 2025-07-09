import { defineConfig } from 'vite'
import RubyPlugin from 'vite-plugin-ruby'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  build: {
    assetsInlineLimit: 24576
  },
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