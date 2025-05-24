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
    <html lang="en" className="dark" style={{ colorScheme: 'dark' }}>
      <head>
        <meta name="color-scheme" content="dark" />
        <script
          dangerouslySetInnerHTML={{
            __html: `
              (function() {
                // Immediately apply dark mode before any rendering
                document.documentElement.classList.add('dark');
                document.documentElement.style.colorScheme = 'dark';
                
                // Override localStorage to prevent any theme switching
                const originalSetItem = localStorage.setItem;
                localStorage.setItem = function(key, value) {
                  if (key.includes('theme') || key.includes('dark') || key.includes('mode')) {
                    return originalSetItem.call(this, key, 'dark');
                  }
                  return originalSetItem.call(this, key, value);
                };
                
                // Override any attempts to remove dark class
                const originalRemove = document.documentElement.classList.remove;
                document.documentElement.classList.remove = function(className) {
                  if (className === 'dark') {
                    return;
                  }
                  return originalRemove.call(this, className);
                };
              })();
            `,
          }}
        />
      </head>
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased bg-gray-900 text-gray-100`}
        style={{ backgroundColor: '#111827', color: '#f3f4f6' }}
      >
        <DarkModeEnforcer />
        <Layout>
          {children}
        </Layout>
      </body>
    </html>
  );
}
