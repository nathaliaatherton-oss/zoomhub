import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  root: '.', 
  publicDir: 'public',
  build: {
    outDir: 'build',
    emptyOutDir: true,
  },
  server: {
    port: 8080,
  },
  envPrefix: 'VITE_',
  resolve: {
    alias: {
      // Map the Haskell source for Tailwind purging
      '/__DELETE_ME__/haskell': '../src',
    },
  },
})