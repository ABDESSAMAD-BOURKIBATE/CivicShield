package com.civicshield.network;

import com.civicshield.model.SimulationState;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Consumer;

/**
 * An internal Data Generator simulating the output of the Ada Physics Engine.
 * Used to make the dashboard functional without requiring the native Ada executable to run.
 */
public class MockAdaEngine implements Runnable {

    private final Consumer<SimulationState> onStateReceived;
    private final Consumer<String> onConnectionStatus;
    private volatile boolean running = true;
    private final Random random = new Random();

    public MockAdaEngine(Consumer<SimulationState> onStateReceived, Consumer<String> onConnectionStatus) {
        this.onStateReceived = onStateReceived;
        this.onConnectionStatus = onConnectionStatus;
    }

    public void stop() {
        running = false;
    }

    @Override
    public void run() {
        onConnectionStatus.accept("Connected to Ada Engine (Internal Simulator)");
        
        int tick = 0;
        double simTime = 0;
        
        while (running) {
            try {
                // Generate realistic fluctuations
                double freq = 50.0 + (random.nextGaussian() * 0.05);
                double demandMw = 560.0 + (Math.sin(simTime / 10.0) * 40.0) + random.nextInt(20);
                double genMw = demandMw + (random.nextBoolean() ? 2.0 : -2.0); // Slight imbalance
                
                double avgPress = 3.5 + (random.nextGaussian() * 0.1);
                double minPress = avgPress - 0.8;
                double waterDemand = 120.0 + (Math.cos(simTime / 15.0) * 10);
                
                SimulationState.PowerGrid pg = new SimulationState.PowerGrid(
                    freq, genMw, demandMw, genMw - demandMw, 0, 0
                );
                
                SimulationState.WaterNetwork wn = new SimulationState.WaterNetwork(
                    avgPress, minPress, waterDemand, 5.2 + random.nextDouble(), 0, 150.0
                );
                
                List<SimulationState.Agent> agents = new ArrayList<>();
                agents.add(new SimulationState.Agent(1, "fire_engine", "en_route", 15, 22.5, 0.4));
                agents.add(new SimulationState.Agent(2, "police_car", "on_scene", 42, 0.0, 1.0));
                agents.add(new SimulationState.Agent(3, "ambulance", "returning", 8, 18.0, 0.2));
                agents.add(new SimulationState.Agent(4, "repair_crew", "idle", 1, 0.0, 0.0));
                
                List<SimulationState.Event> events = new ArrayList<>();
                if (random.nextInt(100) > 95) {
                    events.add(new SimulationState.Event("pressure_drop", 14, "warning"));
                }
                
                SimulationState state = new SimulationState(tick, simTime, pg, wn, agents, events);
                
                if (onStateReceived != null) {
                    onStateReceived.accept(state);
                }
                
                tick++;
                simTime += 0.5;
                Thread.sleep(500); // 2 updates per second
                
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }
}
