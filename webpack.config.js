var path = require('path');

// var MODE =
//     process.env.npm_lifecycle_event === "prod" ? "production" : "development";
// var withDebug = !process.env["npm_config_nodebug"];
// this may help for Yarn users
// var withDebug = !npmParams.includes("--nodebug");
// console.log('\x1b[36m%s\x1b[0m', `** elm-webpack-starter: mode "${MODE}", withDebug: ${withDebug}\n`);

module.exports = {
    entry: './src/static/index.js',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'bundle.js'
    },
    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use : {
                    loader:  'elm-webpack-loader',
                    options: { verbose: true, debug: true }
                } 
            },
            {
                test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: "file-loader"
            },
            {
                test: /\.scss$/,
                exclude: [/elm-stuff/, /node_modules/],
                loaders: ["style-loader", "css-loader?url=false", "sass-loader"]
            },
            {
                test: /\.css$/,
                exclude: [/elm-stuff/, /node_modules/],
                loaders: ["style-loader", "css-loader?url=false"]
            }
        ]
    },
    resolve: {
        extensions: ['*', '.js', '.elm']
    },
    devServer: {
        inline: true,
        contentBase: path.join(__dirname, './src/static'),
        compress: true,
        port: 3000
    }
}
