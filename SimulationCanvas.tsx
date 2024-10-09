import { useRef, useEffect } from 'react';

export type GameLoopInitOptions = {
  lastFrame: {
    timestamp: number;
    rectangle: {
      x: number;
      y: number;
    };
  };
};

export const SimulationCanvas = () => {
  const canvasAspectRatio = 4 / 3;
  const canvasSize = 400;
  const canvas = {
    width: canvasSize,
    height: canvasSize / canvasAspectRatio,
    ref: useRef(null),
  };

  const rectangleAspectRatio = 1 / 1;
  const rectangleSize = 20;
  const rectangle = {
    width: rectangleSize,
    height: rectangleSize / rectangleAspectRatio,
    speed: 10,
  };

  const getBlankCanvasCtx = (canvas) => {
    const ctx = canvas.ref.current.getContext('2d');
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    return ctx;
  };

  const gameLoop = (options: GameLoopInitOptions) => (timestamp: number) => {
    const { lastFrame } = options;

    const ctx = getBlankCanvasCtx(canvas);
    const secondsPassed = (timestamp - lastFrame.timestamp) / 1000;

    const x = lastFrame.rectangle.x + rectangle.speed * secondsPassed;
    const y = lastFrame.rectangle.y + rectangle.speed * secondsPassed;

    ctx.fillStyle = 'ff8080';
    ctx.fillRect(x, y, rectangle.width, rectangle.height);

    window.requestAnimationFrame(
      gameLoop({
        ...options,
        lastFrame: { ...options.lastFrame, timestamp, rectangle: { x, y } },
      })
    );
  };

  useEffect(() => {
    window.requestAnimationFrame(
      gameLoop({
        lastFrame: { timestamp: performance.now(), rectangle: { x: 0, y: 0 } },
      })
    );
  }, []);

  return <canvas ref={canvas.ref}></canvas>;
};
