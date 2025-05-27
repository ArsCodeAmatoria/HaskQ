import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import "./globals.css";
import Layout from "../components/layout/Layout";
import DarkModeEnforcer from "../components/DarkModeEnforcer";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "HaskQ - Quantum Circuits, Purely Functional",
  description: "A functional quantum programming toolkit built with Haskell",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" className="dark" style={{ colorScheme: 'dark' }} data-theme="dark">
      <head>
        <meta name="color-scheme" content="dark" />
        <style dangerouslySetInnerHTML={{
          __html: `
            /* Force dark mode immediately */
            :root { color-scheme: dark !important; }
            html { color-scheme: dark !important; background: #111827 !important; }
            body { background: #111827 !important; color: #f3f4f6 !important; }
            * { color-scheme: dark !important; }
          `
        }} />
        <script
          dangerouslySetInnerHTML={{
            __html: `
              (function() {
                // Immediately apply dark mode before any rendering
                document.documentElement.classList.add('dark');
                document.documentElement.setAttribute('data-theme', 'dark');
                document.documentElement.style.colorScheme = 'dark';
                document.documentElement.style.background = '#111827';
                
                // Override localStorage to prevent any theme switching
                const originalSetItem = localStorage.setItem;
                localStorage.setItem = function(key, value) {
                  if (key.includes('theme') || key.includes('dark') || key.includes('mode')) {
                    return originalSetItem.call(this, key, 'dark');
                  }
                  return originalSetItem.call(this, key, value);
                };
                
                // Override any attempts to remove dark class or data-theme
                const originalRemove = document.documentElement.classList.remove;
                document.documentElement.classList.remove = function(className) {
                  if (className === 'dark') {
                    return;
                  }
                  return originalRemove.call(this, className);
                };
                
                const originalRemoveAttribute = document.documentElement.removeAttribute;
                document.documentElement.removeAttribute = function(name) {
                  if (name === 'data-theme') {
                    return;
                  }
                  return originalRemoveAttribute.call(this, name);
                };
                
                // Override setAttribute to prevent light theme
                const originalSetAttribute = document.documentElement.setAttribute;
                document.documentElement.setAttribute = function(name, value) {
                  if (name === 'data-theme' && value !== 'dark') {
                    return originalSetAttribute.call(this, name, 'dark');
                  }
                  return originalSetAttribute.call(this, name, value);
                };
                
                // Force dark mode on body when it loads
                document.addEventListener('DOMContentLoaded', function() {
                  document.body.style.backgroundColor = '#111827';
                  document.body.style.color = '#f3f4f6';
                });
              })();
            `,
          }}
        />
      </head>
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased bg-gray-900 text-gray-100`}
        style={{ backgroundColor: '#111827', color: '#f3f4f6' }}
        data-theme="dark"
      >
        <DarkModeEnforcer />
        <Layout>
          {children}
        </Layout>
      </body>
    </html>
  );
}
