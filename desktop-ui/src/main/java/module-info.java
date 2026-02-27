module com.civicshield {
    requires javafx.controls;
    requires transitive javafx.graphics;
    requires javafx.fxml;
    requires org.kordamp.ikonli.core;
    requires org.kordamp.ikonli.javafx;
    requires org.kordamp.ikonli.materialdesign2;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.databind;

    requires javafx.swing;
    requires java.desktop;
    requires org.jxmapviewer.jxmapviewer2;
    requires org.knowm.xchart;

    uses org.kordamp.ikonli.IkonHandler;

    opens com.civicshield.ui to javafx.fxml, com.fasterxml.jackson.databind;
    opens com.civicshield.model to com.fasterxml.jackson.databind;

    exports com.civicshield;
}
