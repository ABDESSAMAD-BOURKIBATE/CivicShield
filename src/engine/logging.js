/**
 * CivicShield - Logging Module
 * Centralized, thread-safe event logging with circular buffer (500 entries).
 * Categories: POWER, WATER, TRANSPORT, EMERGENCY, HEALTHCARE, CASCADE, SYSTEM, ACCESS_CTRL
 * Severities: INFO, WARNING, ERROR, CRITICAL
 */

const MAX_LOG_ENTRIES = 500;

const SEVERITY_COLORS = {
  INFO: '#3b82f6',
  WARNING: '#eab308',
  ERROR: '#ef4444',
  CRITICAL: '#dc2626'
};

const SEVERITY_ICONS = {
  INFO: 'ℹ️',
  WARNING: '⚠️',
  ERROR: '❌',
  CRITICAL: '🔴'
};

const CATEGORY_ICONS = {
  POWER: '⚡',
  WATER: '💧',
  TRANSPORT: '🚥',
  EMERGENCY: '🚨',
  HEALTHCARE: '🏥',
  CASCADE: '💥',
  SYSTEM: '🖥️',
  ACCESS_CTRL: '🔑',
  STABILITY: '📊'
};

let logEntries = [];
let logIdCounter = 0;
let currentStep = 0;
let initialized = false;
let onLogCallback = null;

export function initialize() {
  logEntries = [];
  logIdCounter = 0;
  currentStep = 0;
  initialized = true;
}

export function setStep(step) {
  currentStep = step;
}

export function onLog(callback) {
  onLogCallback = callback;
}

export function logEvent(category, severity, message, step = null) {
  if (!initialized) return;

  const entry = {
    id: ++logIdCounter,
    step: step !== null ? step : currentStep,
    category,
    severity,
    message,
    timestamp: new Date().toISOString(),
    categoryIcon: CATEGORY_ICONS[category] || '📋',
    severityIcon: SEVERITY_ICONS[severity] || 'ℹ️',
    severityColor: SEVERITY_COLORS[severity] || '#6b7280'
  };

  logEntries.push(entry);

  if (logEntries.length > MAX_LOG_ENTRIES) {
    logEntries.shift();
  }

  if (onLogCallback) {
    onLogCallback(entry);
  }
}

export function getEntries(count = 50, filter = null) {
  let entries = [...logEntries];

  if (filter) {
    if (filter.category) {
      entries = entries.filter(e => e.category === filter.category);
    }
    if (filter.severity) {
      entries = entries.filter(e => e.severity === filter.severity);
    }
    if (filter.step !== undefined) {
      entries = entries.filter(e => e.step === filter.step);
    }
  }

  return entries.slice(-count);
}

export function getAllEntries() { return [...logEntries]; }
export function getCount() { return logEntries.length; }
export function getCategoryIcons() { return { ...CATEGORY_ICONS }; }
export function getSeverityColors() { return { ...SEVERITY_COLORS }; }

export function formatEntry(entry) {
  return `[Step ${entry.step}] ${entry.category.padEnd(12)} | ${entry.severity.padEnd(8)} | ${entry.message}`;
}
