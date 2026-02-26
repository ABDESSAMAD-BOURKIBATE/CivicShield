/**
 * CivicShield - Healthcare Subsystem Module
 * 4 Hospitals: 750 beds, 105 ICU, +160 surge capacity
 * Surge activates automatically when status reaches Critical/Overwhelmed
 */

const HOSPITAL_CONFIG = [
  { id: 1, name: 'General Hospital Alpha', beds: 200, icu: 30, surge: 50 },
  { id: 2, name: 'City Medical Center', beds: 150, icu: 20, surge: 30 },
  { id: 3, name: 'Regional Hospital Beta', beds: 300, icu: 40, surge: 60 },
  { id: 4, name: 'Community Health Clinic', beds: 100, icu: 15, surge: 20 }
];

let hospitals = [];
let healthcareStatus = 'Normal';
let healthPercentage = 100.0;
let initialized = false;
let patientQueue = [];
let patientIdCounter = 0;

function createHospital(cfg) {
  return {
    ...cfg,
    occupiedBeds: Math.floor(cfg.beds * (0.3 + Math.random() * 0.3)),
    occupiedICU: Math.floor(cfg.icu * (0.2 + Math.random() * 0.3)),
    surgeActive: false,
    surgeBedsUsed: 0,
    isOperational: true,
    staffLevel: 90 + Math.random() * 10
  };
}

export function initialize() {
  hospitals = HOSPITAL_CONFIG.map(cfg => createHospital(cfg));
  healthcareStatus = 'Normal';
  healthPercentage = 100.0;
  patientQueue = [];
  patientIdCounter = 0;
  initialized = true;
}

export function simulateStep() {
  if (!initialized) return;

  for (const h of hospitals) {
    if (!h.isOperational) continue;

    const admissions = Math.floor(Math.random() * 5);
    const discharges = Math.floor(Math.random() * 8);
    h.occupiedBeds = Math.max(0, Math.min(h.beds + (h.surgeActive ? h.surge : 0), h.occupiedBeds + admissions - discharges));

    const icuAdmit = Math.floor(Math.random() * 2);
    const icuDischarge = Math.floor(Math.random() * 3);
    h.occupiedICU = Math.max(0, Math.min(h.icu, h.occupiedICU + icuAdmit - icuDischarge));

    h.staffLevel = Math.max(40, Math.min(100, h.staffLevel + (Math.random() - 0.5) * 5));

    const occupancyRate = h.occupiedBeds / h.beds;
    if (occupancyRate > 0.9 && !h.surgeActive) {
      h.surgeActive = true;
    } else if (occupancyRate < 0.7 && h.surgeActive) {
      h.surgeActive = false;
      h.surgeBedsUsed = 0;
    }
  }

  processPatientQueue();
  updateStatus();
}

export function admitPatient(severity) {
  if (!initialized) return;

  const patient = {
    id: ++patientIdCounter,
    severity,
    admitted: false,
    hospitalId: null
  };

  const targetHospital = hospitals
    .filter(h => h.isOperational)
    .sort((a, b) => {
      const aAvail = a.beds - a.occupiedBeds + (a.surgeActive ? a.surge - a.surgeBedsUsed : 0);
      const bAvail = b.beds - b.occupiedBeds + (b.surgeActive ? b.surge - b.surgeBedsUsed : 0);
      return bAvail - aAvail;
    })[0];

  if (targetHospital) {
    if (severity === 'CRITICAL' && targetHospital.occupiedICU < targetHospital.icu) {
      targetHospital.occupiedICU++;
      patient.admitted = true;
      patient.hospitalId = targetHospital.id;
    } else if (targetHospital.occupiedBeds < targetHospital.beds + (targetHospital.surgeActive ? targetHospital.surge : 0)) {
      targetHospital.occupiedBeds++;
      patient.admitted = true;
      patient.hospitalId = targetHospital.id;
    } else {
      patientQueue.push(patient);
    }
  } else {
    patientQueue.push(patient);
  }

  updateStatus();
}

function processPatientQueue() {
  const remaining = [];
  for (const patient of patientQueue) {
    const h = hospitals.find(h => h.isOperational && h.occupiedBeds < h.beds);
    if (h) {
      h.occupiedBeds++;
      patient.admitted = true;
      patient.hospitalId = h.id;
    } else {
      remaining.push(patient);
    }
  }
  patientQueue = remaining;
}

export function applyDegradation(amount) {
  if (!initialized) return;
  for (const h of hospitals) {
    h.staffLevel = Math.max(20, h.staffLevel - amount * 0.3);
    h.occupiedBeds = Math.min(h.beds + h.surge, h.occupiedBeds + Math.floor(amount * 0.3));
  }
  if (amount > 50) {
    const target = hospitals.find(h => h.isOperational);
    if (target) target.isOperational = false;
  }
  updateStatus();
}

export function applyRecovery() {
  if (!initialized) return;
  const broken = hospitals.find(h => !h.isOperational);
  if (broken) {
    broken.isOperational = true;
    broken.staffLevel = 70;
  }
  for (const h of hospitals) {
    h.staffLevel = Math.min(100, h.staffLevel + 5);
  }
  updateStatus();
}

function updateStatus() {
  const totalBeds = hospitals.reduce((sum, h) => sum + h.beds, 0);
  const occupiedBeds = hospitals.reduce((sum, h) => sum + h.occupiedBeds, 0);
  const totalICU = hospitals.reduce((sum, h) => sum + h.icu, 0);
  const occupiedICU = hospitals.reduce((sum, h) => sum + h.occupiedICU, 0);
  const avgStaff = hospitals.reduce((sum, h) => sum + h.staffLevel, 0) / hospitals.length;
  const operationalPct = hospitals.filter(h => h.isOperational).length / hospitals.length * 100;

  const bedAvailability = Math.max(0, 100 - (occupiedBeds / totalBeds) * 100);
  const icuAvailability = Math.max(0, 100 - (occupiedICU / totalICU) * 100);

  healthPercentage = Math.max(0, Math.min(100,
    operationalPct * 0.3 + bedAvailability * 0.3 + icuAvailability * 0.2 + avgStaff * 0.2
  ));

  if (healthPercentage >= 80) healthcareStatus = 'Normal';
  else if (healthPercentage >= 50) healthcareStatus = 'Warning';
  else if (healthPercentage >= 20) healthcareStatus = 'Critical';
  else healthcareStatus = 'Overwhelmed';
}

export function getStatus() { return healthcareStatus; }
export function getHealthPercentage() { return healthPercentage; }
export function getInfo() {
  const totalBeds = hospitals.reduce((sum, h) => sum + h.beds, 0);
  const occupiedBeds = hospitals.reduce((sum, h) => sum + h.occupiedBeds, 0);
  const totalICU = hospitals.reduce((sum, h) => sum + h.icu, 0);
  const occupiedICU = hospitals.reduce((sum, h) => sum + h.occupiedICU, 0);
  return {
    name: 'Healthcare',
    icon: '🏥',
    status: healthcareStatus,
    health: healthPercentage,
    hospitals: hospitals.map(h => ({ ...h })),
    totalBeds, occupiedBeds, totalICU, occupiedICU,
    surgeActive: hospitals.some(h => h.surgeActive),
    patientQueueLength: patientQueue.length,
    operationalHospitals: hospitals.filter(h => h.isOperational).length,
    totalHospitals: hospitals.length
  };
}
