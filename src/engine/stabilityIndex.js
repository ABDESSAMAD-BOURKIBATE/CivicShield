/**
 * CivicShield - Stability Index Module
 * Computes the Urban Stability Score (USS)
 * USS = Σ (weight_i × health_i)
 * Threat Levels: LOW (≥80), MEDIUM (≥60), HIGH (≥30), CRITICAL (<30)
 */

const WEIGHTS = {
  Power: 0.30,
  Water: 0.25,
  Healthcare: 0.20,
  Emergency: 0.15,
  Transport: 0.10
};

let currentUSS = 100.0;
let currentThreat = 'LOW';
let analysis = {
  uss: 100.0,
  threat: 'LOW',
  weakestSector: 'None',
  healthBreakdown: {},
  weightedContributions: {}
};
let ussHistory = [];
let initialized = false;

export function initialize() {
  currentUSS = 100.0;
  currentThreat = 'LOW';
  ussHistory = [{ step: 0, uss: 100.0, threat: 'LOW' }];
  analysis = {
    uss: 100.0,
    threat: 'LOW',
    weakestSector: 'None',
    healthBreakdown: {},
    weightedContributions: {}
  };
  initialized = true;
}

export function computeScore(healthData) {
  if (!initialized) return;

  const breakdown = {
    Power: Math.max(0, Math.min(100, healthData.power)),
    Water: Math.max(0, Math.min(100, healthData.water)),
    Transport: Math.max(0, Math.min(100, healthData.transport)),
    Emergency: Math.max(0, Math.min(100, healthData.emergency)),
    Healthcare: Math.max(0, Math.min(100, healthData.healthcare))
  };

  const contributions = {};
  let uss = 0;
  for (const [key, weight] of Object.entries(WEIGHTS)) {
    contributions[key] = breakdown[key] * weight;
    uss += contributions[key];
  }

  currentUSS = Math.max(0, Math.min(100, uss));

  if (currentUSS >= 80) currentThreat = 'LOW';
  else if (currentUSS >= 60) currentThreat = 'MEDIUM';
  else if (currentUSS >= 30) currentThreat = 'HIGH';
  else currentThreat = 'CRITICAL';

  let weakest = 'Power';
  let minHealth = breakdown.Power;
  for (const [key, val] of Object.entries(breakdown)) {
    if (val < minHealth) {
      minHealth = val;
      weakest = key;
    }
  }

  analysis = {
    uss: currentUSS,
    threat: currentThreat,
    weakestSector: weakest,
    healthBreakdown: breakdown,
    weightedContributions: contributions
  };

  ussHistory.push({
    step: ussHistory.length,
    uss: currentUSS,
    threat: currentThreat,
    breakdown: { ...breakdown }
  });
}

export function getScore() { return currentUSS; }
export function getThreat() { return currentThreat; }
export function getAnalysis() { return { ...analysis }; }
export function getHistory() { return [...ussHistory]; }
export function getWeights() { return { ...WEIGHTS }; }

export function getThreatColor() {
  switch (currentThreat) {
    case 'LOW': return '#22c55e';
    case 'MEDIUM': return '#eab308';
    case 'HIGH': return '#f97316';
    case 'CRITICAL': return '#ef4444';
    default: return '#6b7280';
  }
}

export function getThreatIcon() {
  switch (currentThreat) {
    case 'LOW': return '🟢';
    case 'MEDIUM': return '🟡';
    case 'HIGH': return '🟠';
    case 'CRITICAL': return '🔴';
    default: return '⚪';
  }
}
