TOTAL_WIDTH = 100;
TOTAL_HEIGHT = 50;

close;

shapesPlot = figure(1);

jsonShapeData = '{"shapes":[{"name":"triangle","vertices":[{"x":10,"y":10},{"x":30,"y":10},{"x":20,"y":20}]},{"name":"square","vertices":[{"x":40,"y":10},{"x":60,"y":10},{"x":60,"y":30},{"x":40,"y":30}]},{"name":"pentagon","vertices":[{"x":70,"y":10},{"x":90,"y":10},{"x":90,"y":30},{"x":80,"y":40},{"x":70,"y":30}]}]}';

shapeData = parseJSON(jsonShapeData);

[dummy, numShapes] = size(shapeData.shapes);

disp(sprintf('Number of Shapes: %d', numShapes));

disp('  Shapes');

clf(shapesPlot);

hold on;

for idxShape = 1:1:numShapes,
    shape = shapeData.shapes{1, idxShape};

    [dummy, numShapeVertices] = size(shape.vertices);

    disp(sprintf('    Nane: %s', shape.name));
    disp(sprintf('    Number of Vertices: %d', numShapeVertices));

    verticesX = zeros(numShapeVertices + 1, 1);
    verticesY = zeros(numShapeVertices + 1, 1);

    disp('    Vertices');

    for idxShapeVertex = 1:1:numShapeVertices,
        shapeVertex = shape.vertices{1, idxShapeVertex};

        verticesX(idxShapeVertex, 1) = shapeVertex.x;
        verticesY(idxShapeVertex, 1) = shapeVertex.y;

        disp(sprintf('      (x: %d y: %d)', shapeVertex.x, shapeVertex.y));
    end

    disp(' ')

    verticesX(numShapeVertices + 1, 1) = verticesX(1, 1);
    verticesY(numShapeVertices + 1, 1) = verticesY(1, 1);

    plot(verticesX, verticesY);
end

axis equal;

axis([0 TOTAL_WIDTH 0 TOTAL_HEIGHT]);

set(shapesPlot, 'Position', [500, 500, TOTAL_WIDTH, TOTAL_HEIGHT]);

hold off;
