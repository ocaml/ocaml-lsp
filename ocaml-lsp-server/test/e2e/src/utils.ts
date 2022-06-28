export const isNotNullable = <T>(
  x: T | null | undefined,
): x is NonNullable<T> => x !== null && x !== undefined;
