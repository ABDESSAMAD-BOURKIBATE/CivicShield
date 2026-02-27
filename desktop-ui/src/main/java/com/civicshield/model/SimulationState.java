package com.civicshield.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

/**
 * Java Record mapping the JSON payload from the Ada Telemetry Engine.
 * Configured to ignore unknown properties for forward compatibility.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public record SimulationState(
    @JsonProperty("tick") int tick,
    @JsonProperty("sim_time_s") double simTimeS,
    @JsonProperty("power_grid") PowerGrid powerGrid,
    @JsonProperty("water_network") WaterNetwork waterNetwork,
    @JsonProperty("agents") List<Agent> agents,
    @JsonProperty("events") List<Event> events
) {

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record PowerGrid(
        @JsonProperty("frequency_hz") double frequencyHz,
        @JsonProperty("total_gen_mw") double totalGenMw,
        @JsonProperty("total_load_mw") double totalLoadMw,
        @JsonProperty("imbalance_mw") double imbalanceMw,
        @JsonProperty("tripped_lines") int trippedLines,
        @JsonProperty("tripped_gens") int trippedGens
    ) {}

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record WaterNetwork(
        @JsonProperty("avg_pressure_bar") double avgPressureBar,
        @JsonProperty("min_pressure_bar") double minPressureBar,
        @JsonProperty("total_demand_cms") double totalDemandCms,
        @JsonProperty("total_leakage_cms") double totalLeakageCms,
        @JsonProperty("inadequate_nodes") int inadequateNodes,
        @JsonProperty("pump_power_mw") double pumpPowerMw
    ) {}

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Agent(
        @JsonProperty("id") int id,
        @JsonProperty("type") String type,
        @JsonProperty("state") String state,
        @JsonProperty("current_node") int currentNode,
        @JsonProperty("speed_mps") double speedMps,
        @JsonProperty("edge_pos") double edgePos
    ) {}

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record Event(
        @JsonProperty("type") String type,
        @JsonProperty("asset") int asset,
        @JsonProperty("severity") String severity
    ) {}
}
