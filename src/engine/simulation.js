/**
 * CivicShield - Simulation Orchestrator
 * Manages the full 20-step deterministic simulation loop.
 * Coordinates all subsystems, cascade engine, stability index, and logging.
 *
 * Scheduled Failure Timeline:
 *   Step  5: Power Generator #2 offline
 *   Step  8: Water Pump #3 burst
 *   Step 10: Traffic incident at Intersection #5 + patient surge
 */

import * as PowerGrid from './powerGrid.js';
import * as WaterNetwork from './waterNetwork.js';
import * as TransportControl from './transportControl.js';
import * as EmergencyResponse from './emergencyResponse.js';
import * as Healthcare from './healthcare.js';
import * as CascadeFailure from './cascadeFailure.js';
import * as StabilityIndex from './stabilityIndex.js';
import * as Logger from './logging.js';

const MAX_STEPS = 20;
const STEP_INTERVAL_MS = 1500;

let currentStep = 0;
let isRunning = false;
let isPaused = false;
let simulationTimer = null;
let onStepCallback = null;
let onCompleteCallback = null;
let speed = 1;
let stepHistory = [];

export function initializeAll() {
  Logger.initialize();
  Logger.logEvent('SYSTEM', 'INFO', 'CivicShield Simulation Engine initializing...');

  PowerGrid.initialize();
  Logger.logEvent('POWER', 'INFO', 'Power Grid initialized: 5 generators online (1000 MW total capacity)');

  WaterNetwork.initialize();
  Logger.logEvent('WATER', 'INFO', 'Water Network initialized: 6 pumps, 3 reservoirs operational');

  TransportControl.initialize();
  Logger.logEvent('TRANSPORT', 'INFO', 'Transport Control initialized: 12 intersections active');

  EmergencyResponse.initialize();
  Logger.logEvent('EMERGENCY', 'INFO', 'Emergency Response initialized: 15 units (4🚒 + 6🚓 + 5🚑)');

  Healthcare.initialize();
  Logger.logEvent('HEALTHCARE', 'INFO', 'Healthcare initialized: 4 hospitals, 750 beds, 105 ICU');

  CascadeFailure.initialize();
  Logger.logEvent('CASCADE', 'INFO', 'Cascade Failure Engine initialized: 5 dependencies mapped');

  StabilityIndex.initialize();
  Logger.logEvent('STABILITY', 'INFO', 'Stability Index initialized: USS = 100.00 (LOW threat)');

  currentStep = 0;
  isRunning = false;
  isPaused = false;
  stepHistory = [];

  Logger.logEvent('SYSTEM', 'INFO', 'All subsystems initialized successfully');
}

function executeStep() {
  currentStep++;
  Logger.setStep(currentStep);

  PowerGrid.simulateStep();
  WaterNetwork.simulateStep();
  TransportControl.simulateStep();
  EmergencyResponse.simulateStep();
  Healthcare.simulateStep();

  if (currentStep === 5) {
    PowerGrid.injectFailure('2');
    Logger.logEvent('POWER', 'ERROR', 'FAILURE INJECTED: Generator #2 taken offline');

    EmergencyResponse.dispatchEvent('Fire', 'High', 'Power Station Alpha');
    Logger.logEvent('EMERGENCY', 'INFO', 'FIRE unit dispatched to Power Station Alpha');
  }

  if (currentStep === 8) {
    WaterNetwork.injectFailure('3');
    Logger.logEvent('WATER', 'ERROR', 'FAILURE INJECTED: Pump #3 pipe burst simulated');
  }

  if (currentStep === 10) {
    TransportControl.injectIncident('5');
    Logger.logEvent('TRANSPORT', 'ERROR', 'INCIDENT: Intersection #5 disabled');

    Healthcare.admitPatient('CRITICAL');
    Logger.logEvent('HEALTHCARE', 'INFO', 'Patient admitted (Severity: CRITICAL)');

    Healthcare.admitPatient('MODERATE');
    Logger.logEvent('HEALTHCARE', 'INFO', 'Patient admitted (Severity: MODERATE)');
  }

  CascadeFailure.evaluateCascade();
  const impact = CascadeFailure.getImpactReport();
  if (impact.totalCascades > 0) {
    Logger.logEvent('CASCADE', 'WARNING', `Cascade detected: ${impact.totalCascades} propagation(s)`);
    if (impact.waterImpact > 0) Logger.logEvent('CASCADE', 'WARNING', `Power degradation affecting Water Network (-${impact.waterImpact.toFixed(1)}%)`);
    if (impact.transportImpact > 0) Logger.logEvent('CASCADE', 'WARNING', `Power degradation affecting Transport (-${impact.transportImpact.toFixed(1)}%)`);
    if (impact.healthcareImpact > 0) Logger.logEvent('CASCADE', 'WARNING', `Degradation affecting Healthcare (-${impact.healthcareImpact.toFixed(1)}%)`);
    if (impact.emergencyImpact > 0) Logger.logEvent('CASCADE', 'WARNING', `Transport degradation affecting Emergency (-${impact.emergencyImpact.toFixed(1)}%)`);
  }

  CascadeFailure.applyRecovery();
  if (impact.recoveryActions > 0) {
    Logger.logEvent('SYSTEM', 'INFO', `Recovery applied: ${impact.recoveryActions} action(s)`);
  }

  const healthData = {
    power: PowerGrid.getHealthPercentage(),
    water: WaterNetwork.getHealthPercentage(),
    transport: TransportControl.getHealthPercentage(),
    emergency: EmergencyResponse.getHealthPercentage(),
    healthcare: Healthcare.getHealthPercentage()
  };

  StabilityIndex.computeScore(healthData);

  const uss = StabilityIndex.getScore();
  const threat = StabilityIndex.getThreat();
  Logger.logEvent('STABILITY', 'INFO', `USS: ${uss.toFixed(2)} | Threat: ${threat}`);

  const snapshot = {
    step: currentStep,
    power: PowerGrid.getInfo(),
    water: WaterNetwork.getInfo(),
    transport: TransportControl.getInfo(),
    emergency: EmergencyResponse.getInfo(),
    healthcare: Healthcare.getInfo(),
    cascade: CascadeFailure.getImpactReport(),
    stability: StabilityIndex.getAnalysis(),
    logs: Logger.getEntries(10)
  };

  stepHistory.push(snapshot);

  if (onStepCallback) {
    onStepCallback(snapshot);
  }

  if (currentStep >= MAX_STEPS) {
    stopSimulation();
    Logger.logEvent('SYSTEM', 'INFO', 'Simulation complete: 20 steps executed');
    if (onCompleteCallback) {
      onCompleteCallback(getFullReport());
    }
  }
}

export function startSimulation() {
  if (isRunning && !isPaused) return;

  if (currentStep === 0 || currentStep >= MAX_STEPS) {
    initializeAll();
  }

  isRunning = true;
  isPaused = false;

  Logger.logEvent('SYSTEM', 'INFO', `Simulation started (${MAX_STEPS} steps, speed: ${speed}x)`);

  simulationTimer = setInterval(() => {
    if (!isPaused) {
      executeStep();
    }
  }, STEP_INTERVAL_MS / speed);
}

export function pauseSimulation() {
  isPaused = true;
  Logger.logEvent('SYSTEM', 'INFO', 'Simulation paused');
}

export function resumeSimulation() {
  isPaused = false;
  Logger.logEvent('SYSTEM', 'INFO', 'Simulation resumed');
}

export function stopSimulation() {
  if (simulationTimer) {
    clearInterval(simulationTimer);
    simulationTimer = null;
  }
  isRunning = false;
  isPaused = false;
}

export function resetSimulation() {
  stopSimulation();
  currentStep = 0;
  stepHistory = [];
  initializeAll();
  Logger.logEvent('SYSTEM', 'INFO', 'Simulation reset');
  if (onStepCallback) {
    onStepCallback(getCurrentSnapshot());
  }
}

export function stepOnce() {
  if (currentStep >= MAX_STEPS) return;
  if (currentStep === 0) {
    initializeAll();
  }
  executeStep();
}

export function setSpeed(newSpeed) {
  speed = Math.max(0.5, Math.min(5, newSpeed));
  if (simulationTimer && isRunning) {
    clearInterval(simulationTimer);
    simulationTimer = setInterval(() => {
      if (!isPaused) executeStep();
    }, STEP_INTERVAL_MS / speed);
  }
}

export function manualInjectFailure(subsystem, component) {
  switch (subsystem) {
    case 'Power':
      PowerGrid.injectFailure(component);
      Logger.logEvent('POWER', 'ERROR', `Manual failure: Generator #${component} offline`);
      break;
    case 'Water':
      WaterNetwork.injectFailure(component);
      Logger.logEvent('WATER', 'ERROR', `Manual failure: Pump #${component} burst`);
      break;
    case 'Transport':
      TransportControl.injectIncident(component);
      Logger.logEvent('TRANSPORT', 'ERROR', `Manual incident: Intersection #${component}`);
      break;
  }
}

export function manualTriggerRecovery() {
  CascadeFailure.applyRecovery();
  Logger.logEvent('SYSTEM', 'INFO', 'Manual recovery triggered');
}

export function onStep(callback) { onStepCallback = callback; }
export function onComplete(callback) { onCompleteCallback = callback; }

export function getCurrentSnapshot() {
  return {
    step: currentStep,
    maxSteps: MAX_STEPS,
    power: PowerGrid.getInfo(),
    water: WaterNetwork.getInfo(),
    transport: TransportControl.getInfo(),
    emergency: EmergencyResponse.getInfo(),
    healthcare: Healthcare.getInfo(),
    cascade: CascadeFailure.getImpactReport(),
    stability: StabilityIndex.getAnalysis(),
    logs: Logger.getEntries(50)
  };
}

export function getFullReport() {
  return {
    totalSteps: currentStep,
    finalUSS: StabilityIndex.getScore(),
    finalThreat: StabilityIndex.getThreat(),
    ussHistory: StabilityIndex.getHistory(),
    cascadeHistory: CascadeFailure.getCascadeHistory(),
    analysis: StabilityIndex.getAnalysis(),
    dependencyMatrix: CascadeFailure.getDependencyMatrix(),
    stepHistory
  };
}

export function getState() {
  return { isRunning, isPaused, currentStep, maxSteps: MAX_STEPS, speed };
}

export { Logger };
