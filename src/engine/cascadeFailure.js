/**
 * CivicShield - Cascade Failure Engine
 * Models inter-subsystem dependencies with weighted propagation matrix.
 * Cascade triggers when any subsystem health < 50%.
 */

import * as PowerGrid from './powerGrid.js';
import * as WaterNetwork from './waterNetwork.js';
import * as TransportControl from './transportControl.js';
import * as EmergencyResponse from './emergencyResponse.js';
import * as Healthcare from './healthcare.js';

const DEPENDENCY_MATRIX = [
  { source: 'Power', target: 'Water', weight: 0.70, rationale: 'Electric pumps require power' },
  { source: 'Power', target: 'Transport', weight: 0.50, rationale: 'Traffic signals require electricity' },
  { source: 'Power', target: 'Healthcare', weight: 0.60, rationale: 'Hospitals need continuous power' },
  { source: 'Water', target: 'Healthcare', weight: 0.40, rationale: 'Hospitals need clean water' },
  { source: 'Transport', target: 'Emergency', weight: 0.50, rationale: 'Emergency vehicles need passable roads' }
];

const RECOVERY_PRIORITY = [
  { subsystem: 'Power', rate: 10 },
  { subsystem: 'Water', rate: 8 },
  { subsystem: 'Transport', rate: 5 },
  { subsystem: 'Emergency', rate: 5 },
  { subsystem: 'Healthcare', rate: 5 }
];

const CASCADE_THRESHOLD = 50;

let impactReport = {
  powerImpact: 0, waterImpact: 0, transportImpact: 0,
  emergencyImpact: 0, healthcareImpact: 0,
  totalCascades: 0, recoveryActions: 0
};

let initialized = false;
let cascadeHistory = [];

export function initialize() {
  impactReport = {
    powerImpact: 0, waterImpact: 0, transportImpact: 0,
    emergencyImpact: 0, healthcareImpact: 0,
    totalCascades: 0, recoveryActions: 0
  };
  cascadeHistory = [];
  initialized = true;
}

function getSubsystemHealth(name) {
  switch (name) {
    case 'Power': return PowerGrid.getHealthPercentage();
    case 'Water': return WaterNetwork.getHealthPercentage();
    case 'Transport': return TransportControl.getHealthPercentage();
    case 'Emergency': return EmergencyResponse.getHealthPercentage();
    case 'Healthcare': return Healthcare.getHealthPercentage();
    default: return 100;
  }
}

function applySubsystemDegradation(name, amount) {
  switch (name) {
    case 'Power': PowerGrid.applyDegradation(amount); break;
    case 'Water': WaterNetwork.applyDegradation(amount); break;
    case 'Transport': TransportControl.applyDegradation(amount); break;
    case 'Emergency': EmergencyResponse.applyDegradation(amount); break;
    case 'Healthcare': Healthcare.applyDegradation(amount); break;
  }
}

function applySubsystemRecovery(name) {
  switch (name) {
    case 'Power': PowerGrid.applyRecovery(); break;
    case 'Water': WaterNetwork.applyRecovery(); break;
    case 'Transport': TransportControl.applyRecovery(); break;
    case 'Emergency': EmergencyResponse.applyRecovery(); break;
    case 'Healthcare': Healthcare.applyRecovery(); break;
  }
}

export function evaluateCascade() {
  if (!initialized) return;

  let totalCascades = 0;
  const currentImpacts = { Power: 0, Water: 0, Transport: 0, Emergency: 0, Healthcare: 0 };

  for (const dep of DEPENDENCY_MATRIX) {
    const sourceHealth = getSubsystemHealth(dep.source);
    if (sourceHealth < CASCADE_THRESHOLD) {
      const degradation = (CASCADE_THRESHOLD - sourceHealth) * dep.weight;
      currentImpacts[dep.target] += degradation;
      totalCascades++;

      cascadeHistory.push({
        source: dep.source,
        target: dep.target,
        degradation: degradation.toFixed(1),
        sourceHealth: sourceHealth.toFixed(1),
        timestamp: Date.now()
      });
    }
  }

  for (const [subsystem, impact] of Object.entries(currentImpacts)) {
    if (impact > 0) {
      applySubsystemDegradation(subsystem, impact);
    }
  }

  impactReport = {
    powerImpact: currentImpacts.Power,
    waterImpact: currentImpacts.Water,
    transportImpact: currentImpacts.Transport,
    emergencyImpact: currentImpacts.Emergency,
    healthcareImpact: currentImpacts.Healthcare,
    totalCascades,
    recoveryActions: impactReport.recoveryActions
  };
}

export function applyRecovery() {
  if (!initialized) return;

  let recoveryActions = 0;

  for (const recovery of RECOVERY_PRIORITY) {
    const health = getSubsystemHealth(recovery.subsystem);
    if (health < 90) {
      applySubsystemRecovery(recovery.subsystem);
      recoveryActions++;
    }
  }

  impactReport.recoveryActions = recoveryActions;
}

export function getImpactReport() { return { ...impactReport }; }
export function getDependencyMatrix() { return [...DEPENDENCY_MATRIX]; }
export function getCascadeHistory() { return [...cascadeHistory]; }
export function getRecoveryPriority() { return [...RECOVERY_PRIORITY]; }
