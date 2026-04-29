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

    console.log('mode', mode);
    console.log('rails_env', process.env.RAILS_ENV);

    const isBuild = (mode === 'production' && process.env.RAILS_ENV !== 'test') || (mode === 'arthur') || (mode === 'test' && (process.env.VITE_RUBY_AUTO_BUILD === 'false'))

    const elmOptions = isBuild ? {
        // optimize: false, // no `--optimize` option when using elm-optimize-level-2
        // nodeElmCompilerOptions: {
        //     pathToElm: 'node_modules/elm-optimize-level-2/bin/elm-optimize-level-2',
        //     processOpts: "--optimize-speed"
        // }
        optimize: true,
        debug: false,
        nodeElmCompilerOptions: {
            optimize: true,
            verbose: true
        }
    } : {
      //   proof that these options are not respected in the output - no debugger
      // in test mode
      debug: true,
      nodeElmCompilerOptions: {
        verbose: true
      }
    }

    // from https://guide.elm-lang.org/optimization/asset_size.html
    // uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output elm.min.js

    // from https://terser.org/docs/options/

    // from https://discourse.elm-lang.org/t/what-i-ve-learned-about-minifying-elm-code/7632

    const pureFuncs = [ "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"];

    return {
        build: {
            assetsInlineLimit: 24576,
            rollupOptions: {
                onLog: {
                    level: 'debug',
                    log: {
                        plugin: true
                    }
                }
            },
            terserOptions: {
                compress: {
                    pure_funcs: pureFuncs,
                    pure_getters: true,
                    // not sure where this option came from - its not referenced in the article above?
                    // keep_fargs: false,
                    unsafe_comps: true,
                    unsafe: true
                },
                mangle : {
                    reserved: pureFuncs
                }
            }
          },
        plugins: [
            RubyPlugin(),
            elmPlugin(elmOptions)
            // {
            //   name: 'log',
            //   options(options) {
            //       console.log('options', options);
            //   },
            //   outputOptions(outputOptions) {
            //       console.log('outputOptions', outputOptions);
            //   },
            // }
        ]
    }
});