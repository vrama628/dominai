import { PropsWithChildren } from "react";

export default function DocsLayout({ children }: PropsWithChildren) {
  return <div className="prose mx-auto">{children}</div>;
}
