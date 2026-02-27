package com.civicshield.network;

import com.civicshield.model.SimulationState;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.function.Consumer;

/**
 * Connects to the CivicShield Ada Engine via TCP Socket on port 9100.
 * Listens for newline-delimited JSON messages, parses them via Jackson,
 * and notifies listeners.
 */
public class TelemetryClient implements Runnable {

    private static final String HOST = "127.0.0.1";
    private static final int PORT = 9100;

    private final ObjectMapper mapper = new ObjectMapper();
    private final Consumer<SimulationState> onStateReceived;
    private final Consumer<String> onConnectionStatus;
    
    private volatile boolean running = true;

    public TelemetryClient(Consumer<SimulationState> onStateReceived, Consumer<String> onConnectionStatus) {
        this.onStateReceived = onStateReceived;
        this.onConnectionStatus = onConnectionStatus;
    }

    public void stop() {
        running = false;
    }

    @Override
    public void run() {
        while (running) {
            onConnectionStatus.accept("Connecting to Ada Engine...");
            try (Socket socket = new Socket(HOST, PORT);
                 BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {
                
                onConnectionStatus.accept("Connected to " + HOST + ":" + PORT);
                
                String line;
                while (running && (line = reader.readLine()) != null) {
                    if (line.trim().isEmpty()) continue;
                    
                    try {
                        SimulationState state = mapper.readValue(line, SimulationState.class);
                        if (onStateReceived != null) {
                            onStateReceived.accept(state);
                        }
                    } catch (Exception parseEx) {
                        System.err.println("Failed to parse telemetry JSON: " + parseEx.getMessage());
                    }
                }
            } catch (Exception e) {
                onConnectionStatus.accept("Disconnected. Retrying in 2 seconds...");
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    return;
                }
            }
        }
    }
}
