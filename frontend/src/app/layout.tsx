import "./globals.css";
import type { Metadata } from "next";
import { Inter, Space_Grotesk } from "next/font/google";
import Link from "next/link";
import { FaGithub, FaQuestionCircle } from "react-icons/fa";

const inter = Inter({ subsets: ["latin"] });
const space_grotesk = Space_Grotesk({ subsets: ["latin"] });

export const metadata: Metadata = {
  title: "DominAI",
  description: "Write code that plays Dominion",
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en">
      <body className={inter.className}>
        <div
          className={`flex items-center mb-2 bg-gray-200 text-gray-800 ${space_grotesk.className}`}
        >
          <Link
            href="/"
            className="text-3xl px-4 py-2 transition-colors hover:bg-gray-300"
          >
            DominAI
          </Link>
          <div className="grow" />
          <Link
            href="/docs"
            className="text-lg rounded-lg px-4 py-2 transition-colors hover:bg-gray-300"
          >
            Documentation
          </Link>
          <a
            href="https://github.com/vrama628/dominai"
            className="border-l p-2 rounded-full transition-colors hover:bg-gray-300"
          >
            <FaGithub size="1.5em" />
          </a>
        </div>
        {children}
      </body>
    </html>
  );
}
