package com.civicshield.ui;

import com.civicshield.model.SimulationState;
import com.civicshield.network.TelemetryClient;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.layout.StackPane;
import javafx.embed.swing.SwingNode;
import org.kordamp.ikonli.javafx.FontIcon;

import org.jxmapviewer.JXMapViewer;

import org.jxmapviewer.viewer.DefaultTileFactory;
import org.jxmapviewer.viewer.GeoPosition;

import org.knowm.xchart.XYChart;
import org.knowm.xchart.XYChartBuilder;
import org.knowm.xchart.XChartPanel;
import org.knowm.xchart.style.Styler;

import javax.swing.SwingUtilities;
import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

public class DashboardController {

    // Header
    @FXML
    private Label connectionStatus;
    @FXML
    private FontIcon connectionIcon;
    @FXML
    private Label simTimeLabel;

    // Power Grid
    @FXML
    private Label freqLabel;
    @FXML
    private Label imbalanceLabel;
    @FXML
    private Label genMwLabel;
    @FXML
    private Label loadMwLabel;
    @FXML
    private HBox powerAlertBox;
    @FXML
    private Label powerAlertLabel;

    // Water Network
    @FXML
    private Label avgPressLabel;
    @FXML
    private Label minPressLabel;
    @FXML
    private Label waterDemandLabel;
    @FXML
    private Label waterLeakLabel;
    @FXML
    private HBox waterAlertBox;
    @FXML
    private Label waterAlertLabel;

    // Fleet Dispatch & Events
    @FXML
    private VBox agentsContainer;
    @FXML
    private ListView<String> eventLogView;

    // New Map & Chart Containers
    @FXML
    private StackPane mapContainer;
    @FXML
    private StackPane chartContainer;

    private TelemetryClient telemetryClient;

    // Map & Chart Instances
    private JXMapViewer mapViewer;
    private XYChart freqChart;
    private XChartPanel<XYChart> chartPanel;

    // Chart Data
    private List<Double> timeData = new ArrayList<>();
    private List<Double> freqData = new ArrayList<>();

    @FXML
    public void initialize() {
        initMap();
        initChart();
        startTelemetry();
    }

    private void initMap() {
        SwingNode mapNode = new SwingNode();
        mapContainer.getChildren().add(mapNode);

        SwingUtilities.invokeLater(() -> {
            // OSM requires a custom User-Agent and HTTPS
            System.setProperty("http.agent", "CivicShield-App/1.0");
            mapViewer = new JXMapViewer();

            // Fallback just in case constructor with args doesn't exist, we can use the
            // default but with https if possible
            // We use standard TileFactoryInfo to be safe if OSMTileFactoryInfo(String,
            // String) is absent.
            org.jxmapviewer.viewer.TileFactoryInfo info = new org.jxmapviewer.viewer.TileFactoryInfo(1, 19, 19,
                    256, true, true, // tile size is 256 and x/y orientation is true
                    "https://tile.openstreetmap.org",
                    "x", "y", "z") {
                @Override
                public String getTileUrl(int x, int y, int zoom) {
                    zoom = getTotalMapZoom() - zoom;
                    return this.baseURL + "/" + zoom + "/" + x + "/" + y + ".png";
                }
            };

            DefaultTileFactory tileFactory = new DefaultTileFactory(info);
            tileFactory.setThreadPoolSize(8);
            mapViewer.setTileFactory(tileFactory);

            // Center roughly around an infrastructure point
            GeoPosition center = new GeoPosition(34.0522, -118.2437);
            mapViewer.setZoom(5);
            mapViewer.setAddressLocation(center);

            mapNode.setContent(mapViewer);
        });
    }

    private void initChart() {
        SwingNode chartNode = new SwingNode();
        chartContainer.getChildren().add(chartNode);

        SwingUtilities.invokeLater(() -> {
            freqChart = new XYChartBuilder()
                    .width(600).height(200)
                    .title("Live Frequency")
                    .xAxisTitle("Time (s)")
                    .yAxisTitle("Hz")
                    .theme(Styler.ChartTheme.Matlab) // Simple dark/scientific theme base
                    .build();

            freqChart.getStyler().setLegendVisible(false);
            freqChart.getStyler().setToolTipsEnabled(false);
            freqChart.getStyler().setMarkerSize(0);

            // Set Dark Mode Colors
            freqChart.getStyler().setChartBackgroundColor(new Color(21, 27, 44)); // #151b2c
            freqChart.getStyler().setPlotBackgroundColor(new Color(11, 15, 25)); // #0b0f19
            freqChart.getStyler().setChartFontColor(Color.LIGHT_GRAY);
            freqChart.getStyler().setAxisTickLabelsColor(Color.LIGHT_GRAY);

            // Initial Data
            timeData.add(0.0);
            freqData.add(50.0);
            freqChart.addSeries("Freq", timeData, freqData)
                    .setLineColor(new Color(56, 189, 248)); // #38bdf8 Neon Blue

            chartPanel = new XChartPanel<>(freqChart);
            chartNode.setContent(chartPanel);
        });
    }

    private void startTelemetry() {
        telemetryClient = new TelemetryClient(this::updateState, this::updateConnectionStatus);
        Thread telemetryThread = new Thread(telemetryClient);
        telemetryThread.setDaemon(true);
        telemetryThread.start();
    }

    private void updateConnectionStatus(String status) {
        Platform.runLater(() -> {
            connectionStatus.setText(status);
            if (status.startsWith("Connected")) {
                connectionIcon.setIconLiteral("mdi2c-connection");
                connectionIcon.setStyle("-fx-icon-color: #10b981;"); // Green
            } else {
                connectionIcon.setIconLiteral("mdi2c-close-network-outline");
                connectionIcon.setStyle("-fx-icon-color: #ef4444;"); // Red
            }
        });
    }

    private void updateState(SimulationState state) {
        // Run UI updates on the JavaFX Platform thread
        Platform.runLater(() -> {
            // Header
            simTimeLabel.setText(String.format("T+ %.1f s", state.simTimeS()));

            // Power Grid
            SimulationState.PowerGrid pg = state.powerGrid();
            freqLabel.setText(String.format("%.2f Hz", pg.frequencyHz()));
            imbalanceLabel.setText(String.format("%.1f MW", pg.imbalanceMw()));
            genMwLabel.setText(String.format("%.1f MW", pg.totalGenMw()));
            loadMwLabel.setText(String.format("%.1f MW", pg.totalLoadMw()));

            updateMetricColor(freqLabel, pg.frequencyHz() < 49.5 || pg.frequencyHz() > 50.5);

            // Update Chart Data safely
            SwingUtilities.invokeLater(() -> {
                if (freqChart != null && chartPanel != null) {
                    timeData.add(state.simTimeS());
                    freqData.add(pg.frequencyHz());

                    // Keep past 200 points to simulate scrolling window
                    if (timeData.size() > 200) {
                        timeData.remove(0);
                        freqData.remove(0);
                    }
                    freqChart.updateXYSeries("Freq", timeData, freqData, null);
                    chartPanel.repaint();
                }
            });

            if (pg.trippedGens() > 0 || pg.trippedLines() > 0) {
                powerAlertBox.setVisible(true);
                powerAlertLabel
                        .setText(String.format("Tripped: %d Gens | %d Lines", pg.trippedGens(), pg.trippedLines()));
            } else {
                powerAlertBox.setVisible(false);
            }

            // Water Network
            SimulationState.WaterNetwork wn = state.waterNetwork();
            avgPressLabel.setText(String.format("%.2f Bar", wn.avgPressureBar()));
            minPressLabel.setText(String.format("%.2f Bar", wn.minPressureBar()));
            waterDemandLabel.setText(String.format("%.2f m³/s", wn.totalDemandCms()));
            waterLeakLabel.setText(String.format("%.2f m³/s", wn.totalLeakageCms()));

            updateMetricColor(minPressLabel, wn.minPressureBar() < 1.5);

            if (wn.inadequateNodes() > 0) {
                waterAlertBox.setVisible(true);
                waterAlertLabel.setText(String.format("Inadequate Pressure at %d Nodes", wn.inadequateNodes()));
            } else {
                waterAlertBox.setVisible(false);
            }

            // Agents Fleet
            agentsContainer.getChildren().clear();
            for (SimulationState.Agent agent : state.agents()) {
                HBox row = new HBox(8);
                row.getStyleClass().add("agent-item");

                FontIcon icon = new FontIcon(getAgentIcon(agent.type()));
                icon.setIconSize(16);
                icon.setStyle("-fx-icon-color: #94a3b8;");

                Label idLabel = new Label("Unit " + agent.id());
                idLabel.getStyleClass().add("agent-item-id");

                Label statusLabel = new Label(agent.state().toUpperCase());
                statusLabel.getStyleClass().add("agent-item-status");
                if (agent.state().equals("en_route")) {
                    statusLabel.setStyle("-fx-text-fill: #eab308;"); // Amber
                }

                row.getChildren().addAll(icon, idLabel, statusLabel);
                agentsContainer.getChildren().add(row);
            }

            // Events
            for (SimulationState.Event evt : state.events()) {
                String logEntry = String.format("[%.1fs] %s : %s at Asset #%d",
                        state.simTimeS(), evt.severity().toUpperCase(), evt.type(), evt.asset());
                eventLogView.getItems().add(0, logEntry); // Add to top

                // Keep log bounded
                if (eventLogView.getItems().size() > 100) {
                    eventLogView.getItems().remove(100, eventLogView.getItems().size());
                }
            }
        });
    }

    private String getAgentIcon(String type) {
        if (type == null)
            return "mdi2i-information";
        return switch (type.toLowerCase().trim()) {
            case "fire_engine" -> "mdi2f-fire-truck"; // Safe fallback if mdi2f-fire-truck doesn't exist
            case "ambulance" -> "mdi2a-ambulance"; // We know this works
            case "police_car" -> "mdi2c-car"; // More standard
            case "repair_crew" -> "mdi2w-wrench";
            default -> "mdi2a-alert-circle"; // We know this works from powerAlertBox
        };
    }

    private void updateMetricColor(Label label, boolean isCritical) {
        label.getStyleClass().removeAll("metric-critical", "metric-value");
        label.getStyleClass().add(isCritical ? "metric-critical" : "metric-value");
    }
}
