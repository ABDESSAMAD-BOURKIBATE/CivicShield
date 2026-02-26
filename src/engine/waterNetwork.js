/**
 * CivicShield - Water Network Subsystem Module
 * Models 6 pumps, 3 reservoirs, 10 pipelines.
 * Health = pump_uptime * 0.6 + reservoir_level * 0.4
 */

const NUM_PUMPS = 6;
const NUM_RESERVOIRS = 3;
const RESERVOIR_CAPACITY = 1000000;

let pumps = [];
let reservoirs = [];
let networkStatus = 'Normal';
let healthPercentage = 100.0;
let initialized = false;

function createPump(id) {
  return {
    id,
    flowRate: 500 + Math.random() * 200,
    pressure: 80 + Math.random() * 20,
    isOperational: true,
    failureCount: 0
  };
}

function createReservoir(id) {
  return {
    id,
    capacity: RESERVOIR_CAPACITY,
    currentLevel: RESERVOIR_CAPACITY * (0.7 + Math.random() * 0.25),
    fillRate: 50000
  };
}

export function initialize() {
  pumps = [];
  reservoirs = [];
  for (let i = 1; i <= NUM_PUMPS; i++) pumps.push(createPump(i));
  for (let i = 1; i <= NUM_RESERVOIRS; i++) reservoirs.push(createReservoir(i));
  networkStatus = 'Normal';
  healthPercentage = 100.0;
  initialized = true;
}

export function simulateStep() {
  if (!initialized) return;

  for (const pump of pumps) {
    if (pump.isOperational) {
      pump.flowRate = Math.max(200, pump.flowRate + (Math.random() - 0.5) * 50);
      pump.pressure = Math.max(40, Math.min(100, pump.pressure + (Math.random() - 0.5) * 5));
    }
  }

  const activePumps = pumps.filter(p => p.isOperational).length;
  const flowMultiplier = activePumps / NUM_PUMPS;

  for (const res of reservoirs) {
    const consumption = 30000 + Math.random() * 20000;
    const inflow = res.fillRate * flowMultiplier;
    res.currentLevel = Math.max(0, Math.min(res.capacity, res.currentLevel + inflow - consumption));
  }

  updateStatus();
}

export function injectFailure(componentId) {
  if (!initialized) return false;
  const id = parseInt(componentId);
  const pump = pumps.find(p => p.id === id);
  if (pump && pump.isOperational) {
    pump.isOperational = false;
    pump.flowRate = 0;
    pump.pressure = 0;
    pump.failureCount++;
    updateStatus();
    return true;
  }
  return false;
}

export function applyDegradation(amount) {
  if (!initialized) return;
  const activePumps = pumps.filter(p => p.isOperational);
  const pumpsToFail = Math.floor(amount / 20);
  for (let i = 0; i < pumpsToFail && i < activePumps.length; i++) {
    activePumps[i].isOperational = false;
    activePumps[i].flowRate = 0;
    activePumps[i].pressure = 0;
  }
  for (const res of reservoirs) {
    res.currentLevel = Math.max(0, res.currentLevel - (amount / 100) * res.capacity * 0.2);
  }
  updateStatus();
}

export function applyRecovery() {
  if (!initialized) return;
  const offlinePump = pumps.find(p => !p.isOperational);
  if (offlinePump) {
    offlinePump.isOperational = true;
    offlinePump.flowRate = 400;
    offlinePump.pressure = 70;
  }
  updateStatus();
}

function updateStatus() {
  const pumpUptime = pumps.filter(p => p.isOperational).length / NUM_PUMPS * 100;
  const avgReservoirLevel = reservoirs.reduce((sum, r) => sum + (r.currentLevel / r.capacity * 100), 0) / NUM_RESERVOIRS;
  healthPercentage = Math.max(0, Math.min(100, pumpUptime * 0.6 + avgReservoirLevel * 0.4));

  if (healthPercentage >= 80) networkStatus = 'Normal';
  else if (healthPercentage >= 50) networkStatus = 'Warning';
  else if (healthPercentage >= 20) networkStatus = 'Critical';
  else networkStatus = 'Offline';
}

export function getStatus() { return networkStatus; }
export function getHealthPercentage() { return healthPercentage; }
export function getInfo() {
  return {
    name: 'Water Network',
    icon: '💧',
    status: networkStatus,
    health: healthPercentage,
    pumps: [...pumps],
    reservoirs: reservoirs.map(r => ({ ...r })),
    activePumps: pumps.filter(p => p.isOperational).length,
    totalPumps: NUM_PUMPS,
    avgReservoirLevel: reservoirs.reduce((sum, r) => sum + (r.currentLevel / r.capacity * 100), 0) / NUM_RESERVOIRS
  };
}
