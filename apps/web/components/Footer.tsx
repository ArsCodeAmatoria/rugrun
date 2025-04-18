import Link from "next/link";
import { Twitter, Github, MessageSquare } from "lucide-react";

export default function Footer() {
  return (
    <footer className="border-t border-zinc-800 py-8">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 flex flex-col md:flex-row justify-between items-center">
        <p className="text-zinc-500 text-sm">
          &copy; {new Date().getFullYear()} RugRun ($RGRN). All rights reserved.
        </p>
        <div className="flex space-x-6 mt-4 md:mt-0">
          <a href="#" className="text-zinc-500 hover:text-primary transition-colors">
            <Twitter size={20} />
            <span className="sr-only">Twitter</span>
          </a>
          <a href="#" className="text-zinc-500 hover:text-primary transition-colors">
            <MessageSquare size={20} />
            <span className="sr-only">Discord</span>
          </a>
          <a href="#" className="text-zinc-500 hover:text-primary transition-colors">
            <Github size={20} />
            <span className="sr-only">GitHub</span>
          </a>
        </div>
      </div>
    </footer>
  );
} 