package com.civicshield;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.util.Objects;

public class Main extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception {
        Parent root = FXMLLoader
                .load(Objects.requireNonNull(getClass().getResource("/com/civicshield/dashboard.fxml")));

        Scene scene = new Scene(root, 1280, 720);
        // Load SCADA styling
        scene.getStylesheets()
                .add(Objects.requireNonNull(getClass().getResource("/com/civicshield/style.css")).toExternalForm());

        primaryStage.setTitle("CIVICSHIELD - High-Integrity Infrastructure Monitor");
        primaryStage.setScene(scene);
        primaryStage.setMinWidth(1024);
        primaryStage.setMinHeight(768);
        primaryStage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
