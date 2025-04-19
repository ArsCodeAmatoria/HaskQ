# Deploying HaskQ to Vercel

This guide provides instructions for deploying the HaskQ monorepo to Vercel. The monorepo contains three main applications:

1. **Landing Page** - The main website
2. **Documentation** - The Docusaurus-based documentation site
3. **Playground** - The interactive HaskQ playground

## Prerequisites

- A [Vercel](https://vercel.com) account
- [Vercel CLI](https://vercel.com/docs/cli) installed: `npm i -g vercel`
- Git
- Node.js (version 18 or higher)

## Deployment Steps

### 1. Clone the Repository

```bash
git clone https://github.com/haskq/haskq.git
cd haskq
```

### 2. Install Dependencies

```bash
npm install
```

### 3. Build the Project

```bash
npm run build
```

This builds all applications in the monorepo using Turborepo.

### 4. Login to Vercel

```bash
vercel login
```

### 5. Deploy to Vercel

#### Option A: Using the Vercel CLI

```bash
npm run deploy
```

This will use the `vercel deploy --prod` command specified in the root package.json.

#### Option B: Using the Vercel Dashboard

1. Push your changes to GitHub
2. Import your project in the Vercel dashboard
3. Configure the following settings:
   - Framework Preset: Other
   - Root Directory: ./
   - Build Command: `npm run build`
   - Output Directory: .

### 6. Verify the Deployment

After deployment, your applications should be available at:

- Landing Page: `https://[your-vercel-domain]/`
- Documentation: `https://[your-vercel-domain]/docs`
- Playground: `https://[your-vercel-domain]/playground`

## Configuration Files

The deployment uses several configuration files:

### Root `vercel.json`

This file specifies the configuration for the monorepo deployment, including build commands and routing rules.

### Next.js Configuration Files

The Next.js applications (landing page and playground) have specific configurations:

- **Landing Page**: `apps/landing/next.config.js` - Configures the landing page with the root base path.
- **Playground**: `apps/playground/next.config.js` - Configures the playground with the `/playground` base path.

### Docusaurus Configuration

The documentation site uses `apps/docs/docusaurus.config.js` with:
- Base URL set to `/docs/`
- Production URL set to your Vercel domain

## Troubleshooting

### Build Errors

If you encounter build errors:

1. Check the build logs in the Vercel dashboard
2. Ensure all dependencies are correctly installed
3. Verify that the build commands work locally

### Routing Issues

If you're experiencing routing problems:

1. Check the `vercel.json` rewrites configuration
2. Verify the base paths in the app configurations
3. Clear your browser cache

### Static Generation

For Next.js apps, ensure they're configured for static export with:

```javascript
// next.config.js
module.exports = {
  output: 'export'
}
```

## Continuous Deployment

To set up continuous deployment:

1. Connect your GitHub repository to Vercel
2. Configure automatic deployments for your production branch
3. Set up Preview Deployments for pull requests

## Need Help?

If you need assistance with the deployment, please reach out:

- Create an issue on [GitHub](https://github.com/haskq/haskq/issues)
- Contact the HaskQ team 