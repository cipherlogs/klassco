const withNextra = require('nextra')({
  theme: 'nextra-theme-docs',
  themeConfig: './theme.config.jsx',
})

const config = {
  output: "export",
  distDir: "dist",
  images: {unoptimized: true},
  ...withNextra()
};

// module.exports = withNextra()
module.exports = config;
