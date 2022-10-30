const webpack = require('webpack');

module.exports = function override (config, env) {
    console.log('override')
    let loaders = config.resolve
    loaders.fallback = {
        "stream": require.resolve("stream-browserify"),
        "buffer": require.resolve("buffer"),
    }

    config.plugins.push(new webpack.ProvidePlugin({
            process: 'process/browser',
        }))

    return config
}
