/**
 * CivicShield - Emergency Response Subsystem Module
 * Fleet: 4 Fire + 6 Police + 5 Medical = 15 units
 * Health = operational% * 0.5 + available% * 0.5
 */

const UNIT_CONFIG = [
  { type: 'Fire', icon: '🚒', count: 4 },
  { type: 'Police', icon: '🚓', count: 6 },
  { type: 'Medical', icon: '🚑', count: 5 }
];

const TOTAL_UNITS = 15;

let units = [];
let activeEvents = [];
let emergencyStatus = 'Normal';
let healthPercentage = 100.0;
let initialized = false;
let eventIdCounter = 0;

function createUnit(id, type, icon) {
  return {
    id,
    type,
    icon,
    status: 'Available',
    isOperational: true,
    deployedTo: null,
    deploymentTimer: 0
  };
}

export function initialize() {
  units = [];
  activeEvents = [];
  eventIdCounter = 0;
  let uid = 1;
  for (const cfg of UNIT_CONFIG) {
    for (let i = 0; i < cfg.count; i++) {
      units.push(createUnit(uid++, cfg.type, cfg.icon));
    }
  }
  emergencyStatus = 'Normal';
  healthPercentage = 100.0;
  initialized = true;
}

export function simulateStep() {
  if (!initialized) return;

  for (const unit of units) {
    if (unit.status === 'Deployed' && unit.isOperational) {
      unit.deploymentTimer++;
      if (Math.random() < 0.3 || unit.deploymentTimer > 5) {
        unit.status = 'Available';
        unit.deployedTo = null;
        unit.deploymentTimer = 0;
      }
    }
  }

  activeEvents = activeEvents.filter(e => {
    e.timer++;
    return e.timer < 8;
  });

  updateStatus();
}

export function dispatchEvent(eventType, priority, location) {
  if (!initialized) return null;

  const event = {
    id: ++eventIdCounter,
    type: eventType,
    priority,
    location,
    timer: 0,
    assignedUnits: []
  };

  const preferredType = eventType === 'Fire' ? 'Fire' : eventType === 'Crime' ? 'Police' : 'Medical';
  const available = units.filter(u => u.status === 'Available' && u.isOperational && u.type === preferredType);
  const unit = available[0] || units.find(u => u.status === 'Available' && u.isOperational);

  if (unit) {
    unit.status = 'Deployed';
    unit.deployedTo = location;
    unit.deploymentTimer = 0;
    event.assignedUnits.push(unit.id);
  }

  activeEvents.push(event);
  updateStatus();
  return event;
}

export function applyDegradation(amount) {
  if (!initialized) return;
  const opUnits = units.filter(u => u.isOperational);
  const toDisable = Math.floor(amount / 15);
  for (let i = 0; i < toDisable && i < opUnits.length; i++) {
    opUnits[i].isOperational = false;
    opUnits[i].status = 'OutOfService';
  }
  updateStatus();
}

export function applyRecovery() {
  if (!initialized) return;
  const broken = units.find(u => !u.isOperational);
  if (broken) {
    broken.isOperational = true;
    broken.status = 'Available';
  }
  updateStatus();
}

function updateStatus() {
  const operational = units.filter(u => u.isOperational).length;
  const available = units.filter(u => u.status === 'Available' && u.isOperational).length;
  const operationalPct = (operational / TOTAL_UNITS) * 100;
  const availablePct = operational > 0 ? (available / operational) * 100 : 0;
  healthPercentage = Math.max(0, Math.min(100, operationalPct * 0.5 + availablePct * 0.5));

  if (healthPercentage >= 80) emergencyStatus = 'Normal';
  else if (healthPercentage >= 50) emergencyStatus = 'Warning';
  else if (healthPercentage >= 20) emergencyStatus = 'Critical';
  else emergencyStatus = 'Offline';
}

export function getStatus() { return emergencyStatus; }
export function getHealthPercentage() { return healthPercentage; }
export function getInfo() {
  const operational = units.filter(u => u.isOperational).length;
  const available = units.filter(u => u.status === 'Available' && u.isOperational).length;
  const deployed = units.filter(u => u.status === 'Deployed').length;
  return {
    name: 'Emergency Response',
    icon: '🚨',
    status: emergencyStatus,
    health: healthPercentage,
    units: units.map(u => ({ ...u })),
    activeEvents: activeEvents.map(e => ({ ...e })),
    operationalCount: operational,
    availableCount: available,
    deployedCount: deployed,
    totalCount: TOTAL_UNITS,
    byType: UNIT_CONFIG.map(cfg => ({
      type: cfg.type,
      icon: cfg.icon,
      total: cfg.count,
      available: units.filter(u => u.type === cfg.type && u.status === 'Available' && u.isOperational).length,
      operational: units.filter(u => u.type === cfg.type && u.isOperational).length
    }))
  };
}
