import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import "./globals.css";
import Layout from "../components/layout/Layout";
import Script from "next/script";

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
  // Force dark color scheme
  colorScheme: 'dark'
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" className="dark">
      <head>
        <meta name="color-scheme" content="dark" />
        <Script id="enforce-dark-mode" strategy="beforeInteractive">
          {`
            (function() {
              // Always force dark mode
              document.documentElement.classList.add('dark');
              
              // Override any system preference detection
              const mediaQuery = window.matchMedia('(prefers-color-scheme: dark)');
              
              // Handle any system preference change events by enforcing dark mode
              mediaQuery.addEventListener('change', () => {
                document.documentElement.classList.add('dark');
              });
            })();
          `}
        </Script>
      </head>
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased dark:bg-gray-900 dark:text-gray-100`}
      >
        <Layout>
          {children}
        </Layout>
      </body>
    </html>
  );
}
