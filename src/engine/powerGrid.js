/**
 * CivicShield - Power Grid Subsystem Module
 * Models an urban power grid with 5 generators and 8 substations.
 * Health = (online_capacity / total_capacity) * 100
 */

const NUM_GENERATORS = 5;
const CAPACITY_MW = 200;
const TOTAL_CAPACITY = NUM_GENERATORS * CAPACITY_MW;

let generators = [];
let gridStatus = 'Normal';
let healthPercentage = 100.0;
let initialized = false;

function createGenerator(id) {
  return {
    id,
    capacityMW: CAPACITY_MW,
    currentLoad: CAPACITY_MW * (0.5 + Math.random() * 0.3),
    isOnline: true,
    failureCount: 0
  };
}

export function initialize() {
  generators = [];
  for (let i = 1; i <= NUM_GENERATORS; i++) {
    generators.push(createGenerator(i));
  }
  gridStatus = 'Normal';
  healthPercentage = 100.0;
  initialized = true;
}

export function simulateStep() {
  if (!initialized) return;

  for (const gen of generators) {
    if (gen.isOnline) {
      const fluctuation = (Math.random() - 0.5) * 20;
      gen.currentLoad = Math.max(50, Math.min(gen.capacityMW, gen.currentLoad + fluctuation));
    }
  }

  const onlineCount = generators.filter(g => g.isOnline).length;
  if (onlineCount > 0 && onlineCount < NUM_GENERATORS) {
    const totalDemand = generators.reduce((sum, g) => sum + (g.isOnline ? g.currentLoad : 0), 0);
    const onlineGens = generators.filter(g => g.isOnline);
    for (const gen of onlineGens) {
      gen.currentLoad = Math.min(gen.capacityMW * 0.95, totalDemand / onlineCount + Math.random() * 10);
    }
  }

  updateStatus();
}

export function injectFailure(componentId) {
  if (!initialized) return false;
  const id = parseInt(componentId);
  const gen = generators.find(g => g.id === id);
  if (gen && gen.isOnline) {
    gen.isOnline = false;
    gen.currentLoad = 0;
    gen.failureCount++;
    updateStatus();
    return true;
  }
  return false;
}

export function applyDegradation(amount) {
  if (!initialized) return;
  const onlineGens = generators.filter(g => g.isOnline);
  const gensToFail = Math.floor(amount / 25);
  for (let i = 0; i < gensToFail && i < onlineGens.length; i++) {
    onlineGens[i].isOnline = false;
    onlineGens[i].currentLoad = 0;
  }
  updateStatus();
}

export function applyRecovery() {
  if (!initialized) return;
  const offlineGen = generators.find(g => !g.isOnline);
  if (offlineGen) {
    offlineGen.isOnline = true;
    offlineGen.currentLoad = offlineGen.capacityMW * 0.6;
  }
  updateStatus();
}

function updateStatus() {
  const onlineCapacity = generators.filter(g => g.isOnline).reduce((sum, g) => sum + g.capacityMW, 0);
  healthPercentage = Math.max(0, Math.min(100, (onlineCapacity / TOTAL_CAPACITY) * 100));

  if (healthPercentage >= 80) gridStatus = 'Normal';
  else if (healthPercentage >= 50) gridStatus = 'Warning';
  else if (healthPercentage >= 20) gridStatus = 'Critical';
  else gridStatus = 'Offline';
}

export function getStatus() { return gridStatus; }
export function getHealthPercentage() { return healthPercentage; }
export function getGenerators() { return [...generators]; }
export function getInfo() {
  return {
    name: 'Power Grid',
    icon: '⚡',
    status: gridStatus,
    health: healthPercentage,
    generators: [...generators],
    onlineCount: generators.filter(g => g.isOnline).length,
    totalCount: NUM_GENERATORS,
    totalCapacity: TOTAL_CAPACITY,
    currentLoad: generators.filter(g => g.isOnline).reduce((sum, g) => sum + g.currentLoad, 0)
  };
}
