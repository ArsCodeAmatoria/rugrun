import { Request, Response, NextFunction } from 'express';
import { logger } from './logger';

export class ApiError extends Error {
  statusCode: number;
  
  constructor(statusCode: number, message: string) {
    super(message);
    this.statusCode = statusCode;
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }
}

export const errorHandler = (
  err: Error | ApiError,
  req: Request,
  res: Response,
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  next: NextFunction
) => {
  let statusCode = 500;
  let message = 'Internal Server Error';

  // Log the error for debugging
  logger.error(`${err.name}: ${err.message}`);
  if (err.stack) {
    logger.error(err.stack);
  }

  // Handle specific API errors
  if (err instanceof ApiError) {
    statusCode = err.statusCode;
    message = err.message;
  } 
  // Handle validation errors
  else if (err.name === 'ValidationError') {
    statusCode = 400;
    message = err.message;
  }
  // Handle JWT errors
  else if (err.name === 'JsonWebTokenError' || err.name === 'TokenExpiredError') {
    statusCode = 401;
    message = 'Authentication failed';
  }

  // Respond with error details
  res.status(statusCode).json({
    error: {
      message,
      statusCode,
      // Only include stack trace in development
      ...(process.env.NODE_ENV === 'development' && { stack: err.stack }),
    },
  });
}; 