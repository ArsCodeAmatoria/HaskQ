import DocsSidebar from '@/components/docs/Sidebar';

export default function DocsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <div className="container mx-auto px-4">
      <div className="flex flex-col md:flex-row">
        <DocsSidebar />
        <div className="flex-1 min-w-0">
          {children}
        </div>
      </div>
    </div>
  );
} 