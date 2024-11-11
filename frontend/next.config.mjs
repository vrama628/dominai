import createMDX from "@next/mdx";

/** @type {import('next').NextConfig} */
const nextConfig = {
  async rewrites() {
    return [
      {
        source: "/api/:path*",
        destination: "http://localhost:3001/:path*",
      },
    ];
  },
  pageExtensions: ["tsx", "mdx"],
};

const withMDX = createMDX();

export default withMDX(nextConfig);
