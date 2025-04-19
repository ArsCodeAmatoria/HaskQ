# Deploying HaskQ to Vercel

This guide provides instructions for deploying the HaskQ applications to Vercel. The repository contains three main applications:

1. **Landing Page** - The main website
2. **Documentation** - The Docusaurus-based documentation site
3. **Playground** - The interactive HaskQ playground

## Prerequisites

- A [Vercel](https://vercel.com) account
- [Vercel CLI](https://vercel.com/docs/cli) installed: `npm i -g vercel`
- Git
- Node.js (version 18 or higher)

## Deployment Strategy: Separate Projects

For the most reliable deployment, we recommend deploying each application as a separate Vercel project.

### 1. Clone the Repository

```bash
git clone https://github.com/haskq/haskq.git
cd haskq
```

### 2. Install Dependencies

```bash
npm install
```

### 3. Deploy the Documentation Site

```bash
cd apps/docs
vercel --prod
```

During setup:
- Framework: Docusaurus
- Output directory: build

### 4. Deploy the Landing Page

```bash
cd ../landing
vercel --prod
```

During setup:
- Framework: Next.js
- Output directory: .next

### 5. Deploy the Playground

```bash
cd ../playground
vercel --prod
```

During setup:
- Framework: Next.js
- Output directory: .next

### 6. Configure Custom Domains

In the Vercel dashboard, you can configure custom domains for each project:

1. For the landing page, use your root domain (e.g., `haskq.com`)
2. For documentation, use a subdomain (e.g., `docs.haskq.com`)
3. For the playground, use another subdomain (e.g., `playground.haskq.com`)

## Configuration Files

Each application has specific configuration files:

### Docusaurus Configuration

For the documentation site, adjust `apps/docs/docusaurus.config.js`:

```js
module.exports = {
  // ...
  url: 'https://docs.yourdomain.com',
  baseUrl: '/',
  // ...
}
```

### Next.js Configuration for Landing Page

For the landing page, adjust `apps/landing/next.config.js`:

```js
module.exports = {
  reactStrictMode: true,
  // No basePath needed when deployed separately
}
```

### Next.js Configuration for Playground

For the playground, adjust `apps/playground/next.config.js`:

```js
module.exports = {
  reactStrictMode: true,
  // No basePath needed when deployed separately
}
```

## Continuous Deployment

To set up continuous deployment for each project:

1. Connect your GitHub repository to each Vercel project
2. Configure the project settings to deploy a specific directory:
   - For docs: `apps/docs`
   - For landing: `apps/landing`
   - For playground: `apps/playground`
3. Set up automatic deployments for your production branch

## Troubleshooting

### React 18 Context Errors

If you encounter React context errors during build:

1. Ensure your Next.js apps are using the correct version of React and React DOM
2. For component libraries that use React Context, ensure they're compatible with the React version

### Broken Links Between Applications

When your apps are deployed to different domains, you'll need to update any cross-application links to use the full URL:

1. In documentation, update links to the playground to use the full URL
2. In the landing page, update links to docs and playground

## Need Help?

If you need assistance with the deployment, please reach out:

- Create an issue on [GitHub](https://github.com/haskq/haskq/issues)
- Contact the HaskQ team 