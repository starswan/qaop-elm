import { defineConfig } from 'vite';
import RubyPlugin from 'vite-plugin-ruby';
import elmPlugin from 'vite-plugin-elm';

// const isTest = process.env.RAILS_ENV === 'test'
// const isProduction = process.env.NODE_ENV === 'production'
// const isArthur = process.env.NODE_ENV === 'arthur'
// // const isBuild = (process.env.RAILS_ENV !== 'test' && (process.env.NODE_ENV === 'production' || process.env.NODE_ENV === 'arthur'))
// const isBuild= isProduction || isArthur
//
// from https://stackoverflow.com/questions/66389043/how-can-i-use-vite-env-variables-in-vite-config-js
//
export default defineConfig(({ mode }) => {
    // const env = loadEnv(mode, process.cwd());

    const isBuild = (mode === 'production') || (mode === 'arthur') || ((mode === 'test') && (process.env.VITE_RUBY_AUTO_BUILD === 'false'))

    const elmOptions = isBuild ? {
        // optimize: false, // no `--optimize` option when using elm-optimize-level-2
        // nodeElmCompilerOptions: {
        //     pathToElm: 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2',
        //     processOpts: "--optimize-speed"
        // }
        optimize: true,
        debug: false,
        nodeElmCompilerOptions: {
          verbose: true
        }
    } : {
      optimize: false,
      nodeElmCompilerOptions: {
        verbose: true
      }
    }

    return {
        build: {
            assetsInlineLimit: 24576
          },
      plugins: [
        elmPlugin(elmOptions),
        // elmPlugin()
        RubyPlugin()
      ]
    }
});