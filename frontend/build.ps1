#!/usr/bin/env pwsh
# Script to build Next.js project with telemetry disabled

# Set environment variables
$env:NEXT_TELEMETRY_DISABLED = 1

# Clean .next directory
Write-Host "Cleaning .next directory..."
Remove-Item -Path ".next" -Recurse -Force -ErrorAction SilentlyContinue

# Run build
Write-Host "Building Next.js project..."
npm run build