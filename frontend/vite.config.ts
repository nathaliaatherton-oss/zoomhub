import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    port: 8080,
  },
  css: {
    postcss: './postcss.config.js',
  },
  resolve: {
    alias: {
      // Map the Haskell source for Tailwind purging
      '/__DELETE_ME__/haskell': '../src',
    },
  },
})