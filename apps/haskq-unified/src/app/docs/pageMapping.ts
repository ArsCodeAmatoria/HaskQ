import IntroPage from "./intro/content";
import SimulationPage from "./core-concepts/simulation/content";
import QuantumComputingBasicsPage from "./core-concepts/quantum-computing-basics/content";

export interface PageInfo {
  title: string;
  description: string;
  content: () => JSX.Element;
}

const pageMapping: Record<string, PageInfo> = {
  "intro": {
    title: "Introduction to HaskQ",
    description: "Get started with HaskQ, a functional quantum programming toolkit",
    content: IntroPage,
  },
  "core-concepts/simulation": {
    title: "Quantum Simulation",
    description: "Learn about simulating quantum circuits in HaskQ",
    content: SimulationPage,
  },
  "core-concepts/quantum-computing-basics": {
    title: "Quantum Computing Basics",
    description: "Learn the fundamental concepts of quantum computing",
    content: QuantumComputingBasicsPage,
  },
};

export default pageMapping; 