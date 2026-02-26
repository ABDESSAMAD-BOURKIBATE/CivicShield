/**
 * CivicShield - Main Entry Point
 * Renders the login screen and dashboard, orchestrates the simulation.
 */

import * as AccessControl from './src/engine/accessControl.js';
import * as Simulation from './src/engine/simulation.js';
import { renderLogin } from './src/ui/loginPage.js';
import { renderDashboard, updateDashboard, showFinalReport } from './src/ui/dashboard.js';

const app = document.getElementById('app');

AccessControl.initialize();

function showLogin() {
  renderLogin(app, (session) => {
    showDashboard(session);
  });
}

function showDashboard(session) {
  Simulation.initializeAll();

  renderDashboard(app, session, {
    onStart: () => Simulation.startSimulation(),
    onPause: () => Simulation.pauseSimulation(),
    onResume: () => Simulation.resumeSimulation(),
    onReset: () => Simulation.resetSimulation(),
    onStep: () => Simulation.stepOnce(),
    onSpeedChange: (s) => Simulation.setSpeed(s),
    onInjectFailure: (sub, comp) => Simulation.manualInjectFailure(sub, comp),
    onTriggerRecovery: () => Simulation.manualTriggerRecovery(),
    onLogout: () => {
      AccessControl.logout();
      Simulation.stopSimulation();
      showLogin();
    },
    getState: () => Simulation.getState(),
    getSnapshot: () => Simulation.getCurrentSnapshot(),
    hasPermission: (action) => AccessControl.hasPermission(action)
  });

  const initialSnapshot = Simulation.getCurrentSnapshot();
  updateDashboard(initialSnapshot, session);

  Simulation.onStep((snapshot) => {
    updateDashboard(snapshot, session);
  });

  Simulation.onComplete((report) => {
    showFinalReport(report);
  });
}

showLogin();
