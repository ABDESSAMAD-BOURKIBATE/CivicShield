/**
 * CivicShield - Transport Control Subsystem Module
 * Models 12 intersections with signal phases.
 * Health = (100 - avg_congestion) * 0.6 + operational% * 0.4
 */

const NUM_INTERSECTIONS = 12;
const PHASES = ['Green', 'Yellow', 'Red'];

let intersections = [];
let transportStatus = 'Normal';
let healthPercentage = 100.0;
let initialized = false;

function createIntersection(id) {
  return {
    id,
    phase: PHASES[Math.floor(Math.random() * 3)],
    congestion: Math.random() * 30,
    isOperational: true,
    incidentActive: false,
    phaseCycleTimer: Math.floor(Math.random() * 3)
  };
}

export function initialize() {
  intersections = [];
  for (let i = 1; i <= NUM_INTERSECTIONS; i++) intersections.push(createIntersection(i));
  transportStatus = 'Normal';
  healthPercentage = 100.0;
  initialized = true;
}

export function simulateStep() {
  if (!initialized) return;

  for (const ix of intersections) {
    if (ix.isOperational && !ix.incidentActive) {
      ix.phaseCycleTimer++;
      if (ix.phaseCycleTimer >= 3) {
        const currentIdx = PHASES.indexOf(ix.phase);
        ix.phase = PHASES[(currentIdx + 1) % PHASES.length];
        ix.phaseCycleTimer = 0;
      }

      if (ix.phase === 'Red') {
        ix.congestion = Math.min(100, ix.congestion + Math.random() * 8);
      } else if (ix.phase === 'Green') {
        ix.congestion = Math.max(0, ix.congestion - Math.random() * 12);
      } else {
        ix.congestion += (Math.random() - 0.3) * 5;
      }
    } else if (ix.incidentActive) {
      ix.phase = 'Flashing';
      ix.congestion = Math.min(100, ix.congestion + 5);
    }

    ix.congestion = Math.max(0, Math.min(100, ix.congestion));

    if (ix.congestion > 70) {
      for (const neighbor of intersections) {
        if (neighbor.id !== ix.id && Math.abs(neighbor.id - ix.id) <= 2 && neighbor.isOperational) {
          neighbor.congestion = Math.min(100, neighbor.congestion + ix.congestion * 0.3 * Math.random() * 0.3);
        }
      }
    }
  }

  updateStatus();
}

export function injectIncident(intersectionId) {
  if (!initialized) return false;
  const id = parseInt(intersectionId);
  const ix = intersections.find(i => i.id === id);
  if (ix && ix.isOperational) {
    ix.incidentActive = true;
    ix.congestion = Math.min(100, ix.congestion + 40);
    ix.phase = 'Flashing';
    updateStatus();
    return true;
  }
  return false;
}

export function applyDegradation(amount) {
  if (!initialized) return;
  for (const ix of intersections) {
    if (ix.isOperational) {
      ix.congestion = Math.min(100, ix.congestion + amount * 0.5);
      if (amount > 40 && Math.random() < 0.3) {
        ix.isOperational = false;
        ix.phase = 'Flashing';
      }
    }
  }
  updateStatus();
}

export function applyRecovery() {
  if (!initialized) return;
  const broken = intersections.find(i => !i.isOperational || i.incidentActive);
  if (broken) {
    broken.isOperational = true;
    broken.incidentActive = false;
    broken.congestion = Math.max(0, broken.congestion - 20);
    broken.phase = 'Green';
  }
  updateStatus();
}

function updateStatus() {
  const avgCongestion = intersections.reduce((sum, i) => sum + i.congestion, 0) / NUM_INTERSECTIONS;
  const operationalPct = intersections.filter(i => i.isOperational).length / NUM_INTERSECTIONS * 100;
  healthPercentage = Math.max(0, Math.min(100, (100 - avgCongestion) * 0.6 + operationalPct * 0.4));

  if (healthPercentage >= 80) transportStatus = 'Normal';
  else if (healthPercentage >= 50) transportStatus = 'Warning';
  else if (healthPercentage >= 20) transportStatus = 'Critical';
  else transportStatus = 'Offline';
}

export function getStatus() { return transportStatus; }
export function getHealthPercentage() { return healthPercentage; }
export function getInfo() {
  return {
    name: 'Transport Control',
    icon: '🚥',
    status: transportStatus,
    health: healthPercentage,
    intersections: intersections.map(i => ({ ...i })),
    operationalCount: intersections.filter(i => i.isOperational).length,
    totalCount: NUM_INTERSECTIONS,
    avgCongestion: intersections.reduce((sum, i) => sum + i.congestion, 0) / NUM_INTERSECTIONS
  };
}
